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

module Main where 

import Plutus.Contract.Test (Wallet, minLogLevel, w1, w2, w3, mockWalletPaymentPubKeyHash)
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.Contract                    qualified as PC
import Plutus.Trace.Emulator              qualified as Trace
import Ledger                             qualified as L
import Ledger.Ada                         qualified as Ada 
import Ledger.Address                     qualified as Address
import Ledger.Value                       qualified as Value
import Ledger.Typed.Scripts               qualified as Scripts
import Cardano.Node.Emulator.TimeSlot     qualified as TimeSlot

import Control.Lens                       (makeLenses, set, (^.))
import Control.Monad                      (when)
import Control.Monad.Freer.Extras.Log     (LogLevel(..))
import Data.Text                          qualified as T
import Data.Default                       (Default (def))
import Data.Data                          (Data)    
import Test.QuickCheck
import Test.Tasty 
import Test.Tasty.QuickCheck

import GuessGame.Game                     qualified as G

-- ---------------------------------------------------------------------- 
-- Contract Model
-- ---------------------------------------------------------------------- 

-- The model type (state)
data GameModel = GameModel 
  { _gameValue     :: !Integer
  , _currentSecret :: !String 
  , _counter       :: !Integer
  } deriving (Data, Show)

makeLenses ''GameModel 

-- ContractModel instance
instance CM.ContractModel GameModel where 

  -- Lock:  Wallet to lock funds, secret password, prize amount
  -- Guess: Player's wallet, guess string
  data Action GameModel = 
      Lock  Wallet String Integer 
    | Guess Wallet String 
    deriving (Eq, Show, Data)
  
  -- Contract instance keys identify the correct contracts to run (GADT).
  -- WalletKey: Identifies the state structure and contract type (with schema) to run. 
  data ContractInstanceKey GameModel w schema err param where 
    WalletKey :: Wallet -> CM.ContractInstanceKey GameModel () G.GameSchema T.Text ()

  -- Provides a `HandleFun` in `perform` to return the contract instance key's handle.
  -- `StartContract` takes a contract instance key and contract parameter 
  -- (all 3 wallets start the initial contract)
  initialInstances :: [CM.StartContract GameModel]
  initialInstances = (`CM.StartContract` ()) . WalletKey <$> wallets

  -- Match contract instance keys with the actual contract to run.
  instanceContract 
      :: (CM.SymToken -> L.AssetClass) 
      -> CM.ContractInstanceKey GameModel w s e p 
      -> p 
      -> PC.Contract w s e ()
  instanceContract _ WalletKey{} _ = G.contract

  -- Return the correct wallet for each contract instance key.
  -- The returned wallet will run the contract identified by the key.
  instanceWallet :: CM.ContractInstanceKey GameModel w s e p -> Wallet
  instanceWallet (WalletKey w) = w

  -- ------------------
  -- GENERATING ACTIONS 
  -- ------------------

  -- Generate random data for each action. 
  -- This data is passed to the contract endpoints (off-chain code).
  arbitraryAction :: CM.ModelState GameModel -> Gen (CM.Action GameModel)
  arbitraryAction _ = oneof $
    ( Lock  <$> genWallet <*> genGuess <*> genValue ) :
    [ Guess <$> genWallet <*> genGuess ]

  -- ----------------------
  -- MODELLING EXPECTATIONS
  -- ----------------------
  
  -- The initial state of each test case
  -- `_counter` is incremented when the Lock action has run. 
  initialState :: GameModel
  initialState = GameModel 
    { _gameValue     = 0
    , _currentSecret = ""
    , _counter       = 0   
    }

  -- Model how we expect each operation to change the state.
  -- Runs in `Spec` monad which keeps track of wallet tokens and the state.
  nextState :: CM.Action GameModel -> CM.Spec GameModel ()
  nextState (Lock w secret val) = do 
    currentSecret CM.$= secret 
    gameValue     CM.$= val 
    counter       CM.$~ (+1)
    CM.withdraw w $ Ada.lovelaceValueOf val
    CM.wait 2

  nextState (Guess w secret) = do
    correctGuess <- (secret ==) <$> CM.viewContractState currentSecret
    if correctGuess 
      then do
        val <- CM.viewContractState gameValue
        CM.deposit w $ Ada.lovelaceValueOf val 
        gameValue CM.$~ subtract val
        CM.wait 2
      else 
        -- Don't run the contract if we know validation will fail.
        -- The simulation will stop with an error if contract validation fails.
        CM.wait 2

  -- -------------
  -- PRECONDITIONS
  -- -------------
  
  -- Define whether an action can run or not, based on the current state.
  precondition :: CM.ModelState GameModel -> CM.Action GameModel -> Bool 
  precondition s cmd = case cmd of 
    Lock  {} -> count == 0
    Guess {} -> count > 0
    where 
      count = s ^. CM.contractState . counter

  -- ------------------
  -- PERFORMING ACTIONS
  -- ------------------
  
  -- So far we are generating actions, but we have not yet linked them to the contract
  -- they are supposed to test. Need to link actions in a test to the emulator.

  -- Call the correct endpoints corresponding to each action, to run in the emulator.
  -- `handle` lets us find the contract handle associated with each ContractInstanceKey.
  perform 
      :: CM.HandleFun GameModel 
      -> (CM.SymToken -> L.AssetClass) 
      -> CM.ModelState GameModel 
      -> CM.Action GameModel 
      -> CM.SpecificationEmulatorTrace ()
  perform handle _ s cmd = case cmd of 

    -- Calls @"lock"@ endpoint in emulator.
    Lock w secret val -> do 
      Trace.callEndpoint @"lock" (handle $ WalletKey w)
        G.LockArgs 
          { G.laGameParam = gameParam 
          , G.laSecret    = secret
          , G.laValue     = Ada.lovelaceValueOf val
          }
      CM.delay 2

    -- Calls @"guess"@ endpoint in emulator.
    Guess w guess -> do 
      let secret = s ^. CM.contractState . currentSecret 
      if secret == guess 
        then do
          Trace.callEndpoint @"guess" (handle $ WalletKey w)
            G.GuessArgs 
              { G.gaGameParam = gameParam 
              , G.gaSecret    = guess 
              }
          CM.delay 2
        else 
          CM.delay 2

  -- Enable QuickCheck to shrink counter examples.
  shrinkAction _s (Lock w secret val) = 
    [Lock w' secret val  | w'   <- shrinkWallet w ] ++
    [Lock w  secret val' | val' <- shrink val ]
  shrinkAction _s (Guess w guess) = 
    [Guess w' guess | w' <- shrinkWallet w]
  
deriving instance Eq (CM.ContractInstanceKey GameModel w s e params)
deriving instance Show (CM.ContractInstanceKey GameModel w s e params)

-- ---------------------------------------------------------------------- 
-- Specific QuickCheck properties
-- ---------------------------------------------------------------------- 

-- Run with `quickCheck` function
testLock :: Property 
testLock = withMaxSuccess 1 . prop_Game $ 
  CM.actionsFromList [Lock w1 "hunter2" 0]

testGuess1 ::Property 
testGuess1 = withMaxSuccess 1 . prop_Game $ 
  CM.actionsFromList [Lock w1 "hunter2" 100, Guess w2 "hunter2"]

testGuess2 ::Property 
testGuess2 = withMaxSuccess 1 . prop_Game $ 
  CM.actionsFromList [Lock w1 "hunter2" 100, Guess w2 "hunter2"]

-- Gives up due to precondition (cannot call Lock more than once per test)
testDoubleLock :: Property
testDoubleLock = prop_Game $ CM.actionsFromList 
  [ Lock w1 "secret" 100
  , Lock w1 "hunter2" 100 
  ]

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

gameParam :: G.GameParam 
gameParam = G.GameParam 
  (mockWalletPaymentPubKeyHash w1) (TimeSlot.scSlotZeroTime def)

wallets :: [Wallet]
wallets = [w1, w2, w3]

-- ---------------------------------------------------------------------- 
-- Generators (for actions)
-- ---------------------------------------------------------------------- 

genWallet :: Gen Wallet 
genWallet = elements wallets

genGuess :: Gen String
genGuess = elements ["hello", "secret", "cardano22", "goodbye"]

genValue :: Gen Integer
genValue = getNonNegative <$> arbitrary

-- ---------------------------------------------------------------------- 
-- Run property tests
-- ---------------------------------------------------------------------- 

-- More general function `propRunActions` allows the check at the 
-- end of each test to be customized.
prop_Game :: CM.Actions GameModel -> Property
prop_Game = CM.propRunActions_ 

-- Eliminate INFO messages. Ie. quickCheck $ prop_Game' Warning 
prop_Game' :: LogLevel -> CM.Actions GameModel -> Property 
prop_Game' l = CM.propRunActionsWithOptions
                  (set minLogLevel l CM.defaultCheckOptionsContractModel)
                  CM.defaultCoverageOptions
                  (\_ -> pure True)

-- ---------------------------------------------------------------------- 
-- Main
-- ---------------------------------------------------------------------- 

main :: IO ()
main = quickCheck prop_Game 
