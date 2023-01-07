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
{-# LANGUAGE TupleSections         #-}

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
                                          , payToPubKeyTarget )

{-
 - IMPORTANT:
 -   The tests have revealed a `redeem` transaction cannot contain more than 
 -   7 outputs from the script address as inputs. Therefore, the contract model 
 -   is written such that a total of 7 `Pay` actions can occur in a single 
 -   test.
 - -}

-- ---------------------------------------------------------------------- 
-- Utilities 
-- ---------------------------------------------------------------------- 

maxOutputs :: Integer 
maxOutputs = 6

testWallets :: [Wallet]
testWallets = [w1, w2, w3, w4, w5]

-- w1 will receive a payout of 10 Ada 
-- w2 will receive a payout of 20 Ada
escrowParams' :: EscrowParams 
escrowParams' = EscrowParams
  { escrowDeadline = TimeSlot.slotToEndPOSIXTime def 30 
  , escrowTargets  = 
    [ payToPubKeyTarget (mockWalletPaymentPubKeyHash w1) (Ada.lovelaceValueOf 10_000_000)
    , payToPubKeyTarget (mockWalletPaymentPubKeyHash w2) (Ada.lovelaceValueOf 20_000_000)
    ]
  }

escrowParams :: [(Wallet, Integer)] -> EscrowParams
escrowParams tgts = 
  EscrowParams 
    { escrowTargets = 
        [ payToPubKeyTarget (mockWalletPaymentPubKeyHash w) (Ada.lovelaceValueOf n)
        | (w, n) <- tgts
        ]
    , escrowDeadline = TimeSlot.slotToEndPOSIXTime def 30
    }

-- ---------------------------------------------------------------------- 
-- Test Contract
-- ---------------------------------------------------------------------- 

-- Contract that allows us to invoke endpoints repeatedly
testContract :: EscrowParams -> (PC.AsContractError e) => PC.Contract () EscrowSchema e ()
testContract params = 
  PC.selectList 
    [ void $ OffChain.payEp    params 
    , void $ OffChain.redeemEp params
    , void $ OffChain.refundEp params
    ] >> testContract params

-- ---------------------------------------------------------------------- 
-- Model
-- ---------------------------------------------------------------------- 

data Phase = Initial | Running 
  deriving (Eq, Show, Data)

data EscrowModel = EscrowModel
  { _contributions :: Map Wallet V.Value
  , _targets       :: Map Wallet V.Value
  , _deadline      :: Integer 
  , _numOutputs    :: Integer
  , _phase         :: Phase
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
    WalletKey :: Wallet 
              -> CM.ContractInstanceKey EscrowModel () EscrowSchema T.Text EscrowParams 

  -- Start instances dynamically using `startInstances`
  initialInstances :: [CM.StartContract EscrowModel]
  initialInstances = []

  -- Initialise a randomly generated parameter to pass to the actual contract
  startInstances _ (Init wns) = 
    [ CM.StartContract (WalletKey w) (escrowParams wns) | w <- testWallets ]
  startInstances _ _ = []

  -- Return wallet that runs the contract identified by the key
  instanceWallet :: CM.ContractInstanceKey EscrowModel w s e p -> Wallet 
  instanceWallet (WalletKey w) = w

  -- Match contract instance keys with the actual contract to run 
  instanceContract 
      :: (CM.SymToken -> L.AssetClass) 
      -> CM.ContractInstanceKey EscrowModel w s e p 
      -> p
      -> PC.Contract w s e ()
  instanceContract _ WalletKey{} params = testContract params

  -- -------
  -- Actions
  -- -------

  data Action EscrowModel = 
      Init   [(Wallet, Integer)]
    | Pay    Wallet Integer 
    | Redeem Wallet 
    | Refund Wallet
    deriving (Eq, Show, Data)

  arbitraryAction :: CM.ModelState EscrowModel -> Gen (CM.Action EscrowModel)
  arbitraryAction s 
    -- Generate an `Init` action only once at the start of a test
    | s ^. CM.contractState . phase == Initial = Init <$> arbitraryTargets
    | otherwise = 
      -- Generate `Redeem` action only when contributed value >= target value
      frequency $ 
        [ (3, Pay <$> elements testWallets <*> choose (2_000_000, 30_000_000))
        , (1, Refund <$> elements testWallets) 
        ] ++
        [ (1, Redeem <$> elements testWallets) | cv `V.geq` tv ]
    where 
      cs = s ^. CM.contractState . contributions
      ts = s ^. CM.contractState . targets
      cv = foldr (<>) mempty (snd <$> Map.toList cs)
      tv = foldr (<>) mempty (snd <$> Map.toList ts)

  -- ------------------
  -- Performing Actions
  -- ------------------

  -- Link actions to the contract to run in the emulator
  perform h _ s a = case a of 
    -- don't do anything for Init actions
    Init _ -> do 
      return ()

    -- call @"pay-escrow" endpoint in emulator
    Pay w v -> do 
      let outputs = s ^. CM.contractState . numOutputs
      if outputs < maxOutputs
        then do 
          Trace.callEndpoint @"pay-escrow" (h $ WalletKey w) (Ada.lovelaceValueOf v)
          CM.delay 2
        else 
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
    , _targets       = Map.empty
    , _deadline      = 30 -- slot 100
    , _numOutputs    = 0
    , _phase         = Initial
    }
  
  -- Model how we expect each operation to change the state. 
  -- The Spec monad keeps track of wallets, tokens, and state.
  nextState :: CM.Action EscrowModel -> CM.Spec EscrowModel () 
  nextState a = case a of 
    Init wns -> do 
      phase   .= Running 
      targets .= Map.fromList [(w, Ada.lovelaceValueOf n) | (w, n) <- wns]

    Pay w v -> do 
      CM.withdraw w $ Ada.lovelaceValueOf v
      numOutputs %= (+1)
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
  shrinkAction _ (Init tgts) = map Init (shrinkList (\(w,n) -> (w,) <$> shrink n) tgts)
  shrinkAction _ (Pay w n) = [Pay w n' | n' <- shrink n]
  shrinkAction _ _         = []

  -- -------------
  -- Preconditions
  -- -------------

  precondition :: CM.ModelState EscrowModel -> CM.Action EscrowModel -> Bool
  precondition s a = case a of 
    Init tgts -> currentPhase == Initial &&
                and [Ada.lovelaceValueOf n `V.geq` Ada.toValue L.minAdaTxOut | (w,n) <- tgts]

    Redeem w -> let fld = foldr (<>) mempty
                in currentPhase == Running && 
                   fld cs `V.geq` fld ts   &&          -- value meets target
                   curSlot < d             &&          -- deadline not passed
                   isJust (Map.lookup w ts)            -- wallet must be in targets

    Refund w -> currentPhase == Running  &&
                case Map.lookup w cs of 
                  Just _  -> curSlot > d &&            -- deadline must pass
                             isJust (Map.lookup w cs)  -- wallet must have contributed
                  Nothing -> False

    Pay _ v  -> currentPhase == Running &&
                curSlot < d             &&
                outputs < maxOutputs    &&
                Ada.lovelaceValueOf v `V.geq` Ada.toValue L.minAdaTxOut 
    where 
      d  = s ^. CM.contractState . deadline
      cs = s ^. CM.contractState . contributions
      ts = s ^. CM.contractState . targets
      outputs = s ^. CM.contractState . numOutputs
      curSlot = L.getSlot $ s ^. CM.currentSlot
      currentPhase = s ^. CM.contractState . phase 

deriving instance Eq (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

-- ---------------------------------------------------------------------- 
-- Generators
-- ---------------------------------------------------------------------- 

arbitraryTargets :: Gen [(Wallet, Integer)]
arbitraryTargets = do 
  ws <- sublistOf testWallets 
  vs <- infiniteListOf $ choose (2_000_000, 30_000_000)
  return (zip (take 2 ws) vs)

-- ---------------------------------------------------------------------- 
-- QuickCheck Properties
-- ---------------------------------------------------------------------- 

testEscrow1 :: Property 
testEscrow1 = withMaxSuccess 1 . prop_Escrow $ 
  CM.actionsFromList
    [ Pay w1 10_000_000
    ]

testEscrow2 :: Property
testEscrow2 = withMaxSuccess 1 . prop_Escrow $
  CM.actionsFromList
    [ Pay w1 20_000_000
    , Pay w2 10_000_000
    , Redeem w1 
    ]

testEscrow3 :: Property
testEscrow3 = withMaxSuccess 1 . prop_Escrow $
  CM.actionsFromList
    [ Pay w1 20_000_000
    , Pay w2 10_000_000
    , Pay w2 3_000_000
    , Pay w2 2_000_000
    , Pay w2 2_000_000
    , Pay w2 2_000_000
    , Redeem w1
    ]

-- ---------------------------------------------------------------------- 
-- Property
-- ---------------------------------------------------------------------- 

prop_Escrow :: CM.Actions EscrowModel -> Property
prop_Escrow = CM.propRunActions_

