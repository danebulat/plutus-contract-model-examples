{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Spec.Escrow where

import Plutus.Contract.Test               (Wallet, minLogLevel, 
                                           mockWalletPaymentPubKeyHash, 
                                           mockWalletAddress,
                                           w1, w2, w3, w4, w5)
import Plutus.Contract.Test               qualified as CT 
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.Contract.Test.ContractModel.Symbolics qualified as CM
import Plutus.Contract                    qualified as PC
import Plutus.Trace.Emulator              qualified as Trace
import Ledger                             qualified as L
import Plutus.V2.Ledger.Api               qualified as LV2
import Ledger.Ada                         qualified as Ada 
import Ledger.Address                     qualified as Address
import Ledger.Value                       qualified as V 
import Ledger.Typed.Scripts               qualified as Scripts
import Ledger.TimeSlot                    qualified as TimeSlot
import PlutusTx.Numeric qualified as PlutusTx

import Control.Lens                       (makeLenses, set, (^.), (.=), (%=), to)
import Control.Monad                      (when, void)
import Control.Monad.Freer.Extras.Log     (LogLevel(..))
import Data.Text                          qualified as T
import Data.Default                       (Default (def))
import Data.Data                          (Data)    
import Data.Maybe                         (isJust, fromJust)
import Data.Map                           qualified as Map
import Data.Map                           (Map)
import Test.QuickCheck
import Test.Tasty 
import Test.Tasty.QuickCheck

import Escrow.OffChain                    qualified as OffChain
import Escrow.OffChain                    ( EscrowSchema )
import Escrow.OnChain                     qualified as OnChain 
import Escrow.OnChain                     ( EscrowParams(..), EscrowTarget(..)
                                          , payToPubKeyTarget
                                          )

-- ---------------------------------------------------------------------- 
-- Utilities 
-- ---------------------------------------------------------------------- 

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

-- adaToValue :: Integer -> V.Value
-- adaToValue n = Ada.lovelaceValueOf ((*) n million)
--   where million = 1_000_000

-- w1 will receive a payout of 10 Ada 
-- w2 will receive a payout of 20 Ada
escrowParams :: EscrowParams 
escrowParams = EscrowParams
  { escrowDeadline = TimeSlot.slotToEndPOSIXTime def 100
  , escrowTargets  = 
    [ payToPubKeyTarget (mockWalletPaymentPubKeyHash w1) (Ada.lovelaceValueOf 10_000_000)
    , payToPubKeyTarget (mockWalletPaymentPubKeyHash w2) (Ada.lovelaceValueOf 20_000_000)
    ]
  }

-- ---------------------------------------------------------------------- 
-- Test Contract
-- ---------------------------------------------------------------------- 

-- Contract that allows us to invoke endpoints repeatedly
testContract :: (PC.AsContractError e) => PC.Contract () EscrowSchema e ()
testContract = 
  PC.selectList 
    [ void $ OffChain.payEp    escrowParams 
    , void $ OffChain.redeemEp escrowParams
    , void $ OffChain.refundEp escrowParams
    ] >> testContract

-- ---------------------------------------------------------------------- 
-- Model
-- ---------------------------------------------------------------------- 

data EscrowModel = EscrowModel
  { _contributions :: Map Wallet V.Value
  , _targets       :: Map Wallet V.Value
  , _deadline      :: Integer 
  } deriving (Eq, Show, Data)

makeLenses ''EscrowModel

-- ---------------------------------------------------------------------- 
-- Contract Model
-- ---------------------------------------------------------------------- 

instance CM.ContractModel EscrowModel where 
  
  -- ----------------------
  -- Contract Instance Keys 
  -- ----------------------
  
  -- Define contract instance keys
  data ContractInstanceKey EscrowModel w s e params where 
    WalletKey :: Wallet -> CM.ContractInstanceKey EscrowModel () EscrowSchema T.Text ()

  -- Make all wallets start the contract pointed to by WalletKey
  initialInstances :: [CM.StartContract EscrowModel]
  initialInstances = [CM.StartContract (WalletKey w) () | w <- testWallets]

  -- Return wallet that runs the contract identified by the key
  instanceWallet :: CM.ContractInstanceKey EscrowModel w s e p -> Wallet 
  instanceWallet (WalletKey w) = w

  -- Match contract instance keys with the actual contract to run 
  instanceContract 
      :: (CM.SymToken -> L.AssetClass) 
      -> CM.ContractInstanceKey EscrowModel w s e p 
      -> p
      -> PC.Contract w s e ()
  instanceContract _ WalletKey{} _ = testContract

  -- -------
  -- Actions
  -- -------

  data Action EscrowModel = 
      Pay    Wallet Integer 
    | Redeem Wallet 
    | Refund Wallet
    deriving (Eq, Show, Data)

  arbitraryAction :: CM.ModelState EscrowModel -> Gen (CM.Action EscrowModel)
  arbitraryAction _ = frequency 
    [ (3, Pay    <$> elements testWallets <*> choose (2_000_000, 30_000_000))
    , (1, Redeem <$> elements testWallets)
    , (1, Refund <$> elements testWallets)
    ]

  -- ------------------
  -- Performing Actions
  -- ------------------

  -- Link actions to the contract to run in the emulator
  perform h _ _ a = case a of 

    -- call @"pay-escrow" endpoint in emulator
    Pay w v -> do 
      Trace.callEndpoint @"pay-escrow" (h $ WalletKey w) (Ada.lovelaceValueOf v)
      CM.delay 2

    -- call @"redeem-escrow" in the emulator
    Redeem w -> do 
      Trace.callEndpoint @"redeem-escrow" (h $ WalletKey w) ()
      CM.delay 2

    -- call @"refund-escrow" in the emulator
    Refund w -> do 
      Trace.callEndpoint @"refund-escrow" (h $ WalletKey w) ()
      CM.delay 2

  -- -----------------
  -- Modelling Actions
  -- -----------------
  
  -- Initial state
  initialState :: EscrowModel 
  initialState = EscrowModel 
    { _contributions = Map.empty 
    , _targets = Map.fromList 
        [ (w1, Ada.lovelaceValueOf 10_000_000)
        , (w2, Ada.lovelaceValueOf 20_000_000)
        ]
    , _deadline = 100 -- slot 100
    }
  
  -- Model how we expect each operation to change the state. 
  -- The Spec monad keeps track of wallets, tokens, and state.
  nextState :: CM.Action EscrowModel -> CM.Spec EscrowModel () 
  nextState a = case a of 
    Pay w v -> do 
      CM.withdraw w $ Ada.lovelaceValueOf v
      contributions %= Map.insertWith (<>) w (Ada.lovelaceValueOf v)
      CM.wait 2

    Redeem w -> do 
      targets' <- CM.viewContractState targets
      sequence_ [ CM.deposit w' v | (w', v) <- Map.toList targets' ]

      contribs <- CM.viewContractState contributions
      let leftoverValue = 
            Map.foldr (<>) mempty contribs <> 
            PlutusTx.negate (Map.foldr (<>) mempty targets')

      CM.deposit w leftoverValue
      contributions .= Map.empty
      CM.wait 2

    Refund w -> do
      contribs <- CM.viewContractState contributions
      let mValToRefund = Map.lookup w contribs
      case mValToRefund of 
        Just v -> do 
          contributions %= Map.delete w
          CM.deposit w v 
          CM.wait 2
        Nothing -> 
          CM.wait 2

  -- Shrinking
  shrinkAction _ (Pay w n) = [Pay w n' | n' <- shrink n]
  shrinkAction _ _         = []

  -- -------------
  -- Preconditions
  -- -------------

  precondition :: CM.ModelState EscrowModel -> CM.Action EscrowModel -> Bool
  precondition s a = case a of 
    Redeem w -> let fld = foldr (<>) mempty
                in fld cs `V.geq` fld ts &&            -- value meets target
                   curSlot < d           &&            -- deadline not passed
                   isJust (Map.lookup w ts)            -- wallet must be in targets

    Refund w -> case Map.lookup w cs of 
                  Just _  -> curSlot > d &&            -- deadline must pass
                             isJust (Map.lookup w cs)  -- wallet must have contributed
                  Nothing -> False

    Pay _ v  -> curSlot < d &&
                Ada.lovelaceValueOf v `V.geq` Ada.toValue L.minAdaTxOut
    where 
      d  = s ^. CM.contractState . deadline
      cs = s ^. CM.contractState . contributions
      ts = s ^. CM.contractState . targets
      curSlot = L.getSlot $ s ^. CM.currentSlot

deriving instance Eq (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

-- ---------------------------------------------------------------------- 
-- Property
-- ---------------------------------------------------------------------- 

prop_Escrow :: CM.Actions EscrowModel -> Property
prop_Escrow = CM.propRunActions_

