{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ImportQualifiedPost  #-}
{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module GuessGameV2.Emulator where

import Plutus.Trace.Emulator      qualified as Emulator
import Ledger.TimeSlot            qualified as TimeSlot
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
import GuessGameV2.OffChain       qualified as OffChain
import GuessGameV2.OnChain        qualified as OnChain


-- ---------------------------------------------------------------------- 
-- Configuration
-- ---------------------------------------------------------------------- 

-- Wallet 2 starts with the guess token + 1000 ADA
emCfg :: Emulator.EmulatorConfig
emCfg = def {
  Emulator._initialChainState = Left $ Map.fromList
    [ (w1, v)
    , (w2, v <> V.assetClassValue guessToken 1)
    , (w3, v)
    ]
  }

v :: V.Value
v = Ada.lovelaceValueOf 1_000_000_000

w1, w2, w3 :: Wallet
w1 = knownWallet 1
w2 = knownWallet 2
w3 = knownWallet 3

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

gameParam :: OnChain.GameParam 
gameParam = OnChain.GameParam 
  { OnChain.gpCreator   = Wallet.mockWalletAddress $ knownWallet 1 
  , OnChain.gpStartTime = TimeSlot.slotToEndPOSIXTime def 0 
  }

guessTokenCurrency :: LV2.CurrencySymbol
guessTokenCurrency = "b89bbdd6b7e801b90fbd3a249f462fb049d43dd7a87d74a71cc97369"

guessTokenName :: LV2.TokenName 
guessTokenName = "GUESS TOKEN"

guessToken :: V.AssetClass 
guessToken = V.AssetClass (guessTokenCurrency, guessTokenName) 

-- ---------------------------------------------------------------------- 
-- Trace 1
-- ---------------------------------------------------------------------- 

trace1 :: Emulator.EmulatorTrace () 
trace1 = do 
  h1 <- Emulator.activateContractWallet (knownWallet 1) OffChain.contract
  h2 <- Emulator.activateContractWallet (knownWallet 2) OffChain.contract
  h3 <- Emulator.activateContractWallet (knownWallet 3) OffChain.contract

  {- Scenario 1 
   -> Wallet 1 starts a guessing game, deposits 200 ADA
   -  Wallet 2 starts with the guess token 
   -> Wallet 2 correctly guesses the secret and receives 50 ADA
   -  Wallet 2 sends the guess token to Wallet 3
   -> Wallet 3 correctly guesses the secret and receives 30 ADA 
   -  Wallet 3 sends the guess token to Wallet 1
   -}

  Emulator.callEndpoint @"lock" h1 $ OffChain.LockArgs
    { OffChain.laGameParam  = gameParam
    , OffChain.laSecret     = "secret"
    , OffChain.laValue      = Ada.lovelaceValueOf 200_000_000
    , OffChain.laGuessToken = guessToken
    }
  void $ waitNSlots 2

  Emulator.callEndpoint @"guess" h2 $ OffChain.GuessArgs
    { OffChain.gaGameParam        = gameParam 
    , OffChain.gaGuessTokenTarget = Wallet.mockWalletAddress $ knownWallet 3 
    , OffChain.gaOldSecret        = "secret"
    , OffChain.gaNewSecret        = "banana"
    , OffChain.gaValueTakenOut    = Ada.lovelaceValueOf 50_000_000 
    }
  void $ waitNSlots 2

  Emulator.callEndpoint @"guess" h3 $ OffChain.GuessArgs
    { OffChain.gaGameParam        = gameParam 
    , OffChain.gaGuessTokenTarget = Wallet.mockWalletAddress $ knownWallet 1 
    , OffChain.gaOldSecret        = "banana"
    , OffChain.gaNewSecret        = "peach"
    , OffChain.gaValueTakenOut    = Ada.lovelaceValueOf 30_000_000 
    }
  void $ waitNSlots 2 

-- ---------------------------------------------------------------------- 
-- Test
-- ---------------------------------------------------------------------- 

test :: IO ()
test = Emulator.runEmulatorTraceIO' def emCfg trace1
