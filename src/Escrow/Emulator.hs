{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Escrow.Emulator where

import Plutus.Trace.Emulator      qualified as Emulator
import Data.Default               (Default (..))
import Data.Map                   qualified as Map
import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Plutus.Trace
import Plutus.V2.Ledger.Api       qualified as LV2
import Plutus.V1.Ledger.Value     qualified as V
import Ledger                     qualified as L
import Ledger.Ada                 qualified as Ada
import Ledger.TimeSlot            qualified as TimeSlot
import Wallet.Emulator.Wallet     (Wallet, knownWallet, mockWalletPaymentPubKeyHash)
import Wallet.Emulator.Wallet     qualified as Wallet

import Escrow.OffChain            qualified as OffChain
import Escrow.OnChain             qualified as OnChain
import Escrow.OnChain (EscrowParams(..), EscrowTarget(..), payToPubKeyTarget)
import Data.Text (Text)
import Plutus.Contract qualified as PC
import Data.Text qualified as T

-- ---------------------------------------------------------------------- 
-- Configuration
-- ---------------------------------------------------------------------- 

emCfg :: Emulator.EmulatorConfig 
emCfg = def {
  Emulator._initialChainState = Left $ Map.fromList 
    [ (w1, v)
    , (w2, v)
    , (w3, v)
    , (w4, v)
    ]
  }

v :: V.Value 
v = Ada.lovelaceValueOf 100_000_000

w1, w2, w3, w4:: Wallet 
w1 = knownWallet 1
w2 = knownWallet 2
w3 = knownWallet 3
w4 = knownWallet 4

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

targetWallets :: [(L.PaymentPubKeyHash, V.Value)]
targetWallets = 
  [ (mockWalletPaymentPubKeyHash w1, Ada.lovelaceValueOf 20_000_000)
  , (mockWalletPaymentPubKeyHash w2, Ada.lovelaceValueOf 15_000_000) 
  , (mockWalletPaymentPubKeyHash w3, Ada.lovelaceValueOf 10_000_000)
  ]

escrowParam :: EscrowParams
escrowParam = EscrowParams  
  { escrowDeadline = TimeSlot.slotToEndPOSIXTime def 50
  , escrowTargets  = map (uncurry payToPubKeyTarget) targetWallets
  } 

-- ---------------------------------------------------------------------- 
-- Trace 1
-- ---------------------------------------------------------------------- 

type EscrowContract = PC.Contract () OffChain.EscrowSchema T.Text ()

trace1 :: Emulator.EmulatorTrace ()
trace1 = do 
  -- Wallet 1 and 2 will pay to the script address
  h1 <- Emulator.activateContractWallet w1
           (OffChain.escrowContract escrowParam :: EscrowContract)
  h2 <- Emulator.activateContractWallet w2
           (OffChain.escrowContract escrowParam :: EscrowContract)
  h3 <- Emulator.activateContractWallet w3
           (OffChain.escrowContract escrowParam :: EscrowContract)

  -- Pay enough funds to contract
  Emulator.callEndpoint @"pay-escrow" h1 (Ada.lovelaceValueOf 20_000_000)
  void $ waitNSlots 2
  Emulator.callEndpoint @"pay-escrow" h2 (Ada.lovelaceValueOf 25_000_000)
  void $ waitNSlots 6

  -- Wallet 3 will redeem 
  Emulator.callEndpoint @"redeem-escrow" h3 ()
  void $ waitNSlots 3

-- ---------------------------------------------------------------------- 
-- Test
-- ---------------------------------------------------------------------- 

test :: IO ()
test = Emulator.runEmulatorTraceIO' def emCfg trace1
