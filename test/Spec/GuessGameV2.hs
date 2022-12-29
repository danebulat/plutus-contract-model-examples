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

module Spec.GuessGameV2 
  ( testsIO
  , testsIO'
  , tests
  ) where 

import Plutus.Contract.Test               (Wallet, minLogLevel, mockWalletPaymentPubKeyHash, 
                                           mockWalletAddress)
import Plutus.Contract.Test               qualified as CT 
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.Contract                    qualified as PC
import Plutus.Trace.Emulator              qualified as Trace
import Ledger                             qualified as L
import Plutus.V2.Ledger.Api               qualified as LV2
import Ledger.Ada                         qualified as Ada 
import Ledger.Address                     qualified as Address
import Ledger.Value                       qualified as V 
import Ledger.Typed.Scripts               qualified as Scripts
import Ledger.TimeSlot                    qualified as TimeSlot

import Control.Lens                       (makeLenses, set, (^.))
import Control.Monad                      (when)
import Control.Monad.Freer.Extras.Log     (LogLevel(..))
import Data.Text                          qualified as T
import Data.Default                       (Default (def))
import Data.Data                          (Data)    
import Data.Maybe                         (fromJust)
import Test.QuickCheck
import Test.Tasty 
import Test.Tasty.QuickCheck

import GuessGameV2.OffChain               qualified as G
import GuessGameV2.OnChain                qualified as OnChain 

-- ---------------------------------------------------------------------- 
-- Contract Model
-- ---------------------------------------------------------------------- 

data GameModel = GameModel 
  { _gameValue     :: !Integer 
  , _currentSecret :: !String 
  , _counter       :: !Integer 
  , _creator       :: Maybe Wallet
  , _tokenHolder   :: Maybe Wallet
  } deriving (Data, Show)

makeLenses ''GameModel

instance CM.ContractModel GameModel where 

  -- Lock:  W1 locks funds, W2 receives guess token, secret, lovelace
  -- Give:  W1 sends guess token, W2 receives guess token 
  -- Guess: W1 makes guess, W2 receives guess token, guess, next secret, 
  --        value to extract from contract 
  data Action GameModel =
      Lock  Wallet Wallet String Integer   
    | Give  Wallet Wallet  
    | Guess Wallet Wallet String String Integer
    deriving (Eq, Show, Data)

  -- Define contract instance keys 
  data ContractInstanceKey GameModel w schema err param where 
    WalletKey :: Wallet -> CM.ContractInstanceKey GameModel () G.GameSchema T.Text ()

  -- Provides machinery in `perform` to return the contract instance key handle. 
  -- StartContract takes a contract instance key and a contract parameter.
  -- Make all three wallets start the initial contract.
  initialInstances :: [CM.StartContract GameModel]
  initialInstances = (`CM.StartContract` ()) . WalletKey <$> wallets

  -- Match contract instance keys with the actual contract to run.
  instanceContract 
      :: (CM.SymToken -> L.AssetClass) 
      -> CM.ContractInstanceKey GameModel w s e p 
      -> p 
      -> PC.Contract w s e () 
  instanceContract _ WalletKey{} _ = G.contract

  -- Return the wallet for each contract instance key that will run the 
  -- contract identified by the key.
  instanceWallet :: CM.ContractInstanceKey GameModel w s e p -> Wallet 
  instanceWallet (WalletKey w) = w

  -- ------------------
  -- GENERATING ACTIONS
  -- ------------------

  -- Generate random data for each action.
  -- This data is passed to the contract endpoints (off-chain code) 
  arbitraryAction :: CM.ModelState GameModel -> Gen (CM.Action GameModel)
  arbitraryAction _ = frequency 
    [ (2,  Lock  <$> genWallet <*> genWallet <*> genGuess <*> genLargeValueInRange)
    , (5,  Give  <$> genWallet <*> genWallet)
    , (14, Guess <$> genWallet <*> genWallet <*> genGuess <*> genGuess <*> genSmallValueInRange)]

  -- ----------------------
  -- MODELLING EXPECTATIONS
  -- ----------------------

  -- The initial state of each test case.
  initialState :: GameModel 
  initialState = GameModel 
    { _gameValue     = 0 
    , _currentSecret = ""
    , _counter       = 0 
    , _creator       = Nothing
    , _tokenHolder   = Nothing
    }

  -- Model how we expect each operation to change the state. 
  -- The Spec monad keeps track of wallets, tokens, and state.
  nextState :: CM.Action GameModel -> CM.Spec GameModel () 
  nextState (Lock w1 w2 secret val) = do 
    currentSecret CM.$= secret 
    gameValue     CM.$= val 
    counter       CM.$~ (+1)
    creator       CM.$= Just w1
    tokenHolder   CM.$= Just w2

    CM.mint        $ V.assetClassValue guessToken 1
    CM.withdraw w1 $ Ada.lovelaceValueOf val <> minLovelace
    CM.deposit  w2 $ V.assetClassValue guessToken 1 <> minLovelace
    CM.wait 2 

  nextState (Give w1 w2) = do 
    -- Precondition: w1 has to hold the guess token
    tokenHolder CM.$= Just w2
    CM.withdraw w1 $ V.assetClassValue guessToken 1 <> minLovelace
    CM.deposit  w2 $ V.assetClassValue guessToken 1 <> minLovelace
    CM.wait 2

  nextState (Guess w1 w2 guess newSecret valToExtract) = do 
    -- Precondition: Don't run if `valToExtract` >= gameValue 
    -- Precondition: w1 has to hold the guess token
    correctGuess <- (guess ==) <$> CM.viewContractState currentSecret
    if correctGuess 
      then do 
        gameValue     CM.$~ subtract valToExtract
        currentSecret CM.$= newSecret
        tokenHolder   CM.$= Just w2

        CM.withdraw w1 minLovelace 
        CM.deposit  w1 $ Ada.lovelaceValueOf valToExtract 
        CM.transfer w1 w2 $ V.assetClassValue guessToken 1 <> minLovelace

        CM.wait 2
      else 
        CM.wait 2
        
  -- -------------
  -- PRECONDITIONS
  -- -------------

  -- Define whether an action can run or not, based on the current state 
  precondition :: CM.ModelState GameModel -> CM.Action GameModel -> Bool 
  precondition s cmd = case cmd of 
      Lock  w1 w2 _ v -> 
           count == 0 && w1 /= w2 && v >= minLove

      Guess w1 w2 _ _ valToExtract -> 
        count > 0 && 
        valToExtract <= curVal &&
        tokHolder == Just w1 &&
        w1 /= w2  -- Must change guess token recipient

      Give w1 _ -> 
        count > 0 && 
        tokHolder == Just w1
    where 
      minLove   = Ada.getLovelace $ Ada.fromValue minLovelace
      count     = s ^. CM.contractState . counter
      curVal    = s ^. CM.contractState . gameValue
      tokHolder = s ^. CM.contractState . tokenHolder

  -- ------------------
  -- PERFORMING ACTIONS 
  -- ------------------

  -- So far we are generating actions, but we have not yet linked them to the contract
  -- they are supposed to test. We need to link actions in a test to the emulator.

  -- Call the correct endpoints corresponding to each action in run in the emulator. 
  perform
      :: CM.HandleFun GameModel 
      -> (CM.SymToken -> L.AssetClass)
      -> CM.ModelState GameModel 
      -> CM.Action GameModel 
      -> CM.SpecificationEmulatorTrace ()
  perform handle _ s cmd = case cmd of 

    -- call @"lock"@ endpoint in emulator 
    Lock w1 w2 secret val -> do 
      Trace.callEndpoint @"lock" (handle $ WalletKey w1)
        G.LockArgs 
          { G.laGameParam  = gameParam w1
          , G.laSecret     = secret
          , G.laValue      = Ada.lovelaceValueOf val 
          , G.laGuessToken = guessToken
          , G.laRecipient  = mockWalletAddress w2
          }
      CM.delay 2
    
    -- call @"give"@ endpoint in emulator 
    Give w1 w2 -> do 
      let tokenHolder' = s ^. CM.contractState . tokenHolder
      if Just w1 == tokenHolder' 
        then do 
          Trace.callEndpoint @"give" (handle $ WalletKey w1) 
            G.GiveArgs 
              { G.gvRecipient  = mockWalletAddress w2 
              , G.gvGuessToken = guessToken 
              }
          CM.delay 2
        else 
          CM.delay 2

    -- call @"guess"@ endpoint in emulator 
    Guess w1 w2 guess nextSecret val -> do 
      let secret       = s ^. CM.contractState . currentSecret
          tokenHolder' = s ^. CM.contractState . tokenHolder

      if secret == guess && tokenHolder' == Just w1
        then do 
          let creator' = fromJust $ s ^. CM.contractState . creator 
          Trace.callEndpoint @"guess" (handle $ WalletKey w1)
            G.GuessArgs 
              { G.gaGameParam        = gameParam creator' 
              , G.gaGuessTokenTarget = mockWalletAddress w2 
              , G.gaOldSecret        = guess 
              , G.gaNewSecret        = nextSecret 
              , G.gaValueTakenOut    = Ada.lovelaceValueOf val
              }
          CM.delay 2
        else 
          CM.delay 2

  -- Enable QuickCheck to shrink counter examples.
  shrinkAction _s (Lock w1 w2 secret val) =
    [Lock w' w2 secret  val  | w'   <- shrinkWallet w1] ++
    [Lock w1 w'' secret val  | w''  <- shrinkWallet w2] ++
    [Lock w1 w2 secret  val' | val' <- shrink val]
  shrinkAction _s (Give w1 w2) =
    [Give w' w2  | w'  <- shrinkWallet w1] ++
    [Give w1 w'' | w'' <- shrinkWallet w2]
  shrinkAction _s (Guess w1 w2 old new val) =
    [Guess w' w2  old new val  | w'   <- shrinkWallet w1] ++
    [Guess w1 w'' old new val  | w''  <- shrinkWallet w2] ++
    [Guess w1 w2  old new val' | val' <- shrink val]

deriving instance Eq (CM.ContractInstanceKey GameModel w s e params)
deriving instance Show (CM.ContractInstanceKey GameModel w s e params)

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

wallets :: [Wallet]
wallets = [CT.w1, CT.w2, CT.w3]

gameParam :: Wallet -> OnChain.GameParam 
gameParam w = OnChain.GameParam 
  { OnChain.gpCreator   = mockWalletAddress w
  , OnChain.gpStartTime = TimeSlot.slotToEndPOSIXTime def 0 
  }

guessTokenCurrency :: LV2.CurrencySymbol
guessTokenCurrency = OnChain.freeCurSymbol

guessTokenName :: LV2.TokenName 
guessTokenName = "GUESS TOKEN"

guessToken :: V.AssetClass 
guessToken = V.AssetClass (guessTokenCurrency, guessTokenName) 

minLovelace :: V.Value
minLovelace = Ada.lovelaceValueOf (Ada.getLovelace  L.minAdaTxOut)

-- ---------------------------------------------------------------------- 
-- Generators (for actions)
-- ---------------------------------------------------------------------- 

genWallet :: Gen Wallet 
genWallet = elements wallets

genGuess :: Gen String
genGuess = elements ["hello", "secret", "cardano", "goodbye"]

genValue :: Gen Integer
genValue = getNonNegative <$> arbitrary

genLargeValueInRange :: Gen Integer 
genLargeValueInRange = choose (75_000_000, 90_000_000)

genSmallValueInRange :: Gen Integer 
genSmallValueInRange = choose (2_000_000, 10_000_000)

-- ---------------------------------------------------------------------- 
-- QuickCheck functions
-- ---------------------------------------------------------------------- 

-- Remove minimum ada precondition for Lock action to see sequences of actions.
sampleActions :: IO ()
sampleActions = sample (arbitrary :: Gen (CM.Actions GameModel))

testLock :: Property 
testLock = withMaxSuccess 1 . prop_Game $ 
  CM.actionsFromList 
    [ Lock CT.w1 CT.w2 "secret" 10_000_000 ]

testGuess1 :: Property 
testGuess1 = withMaxSuccess 1 . prop_Game $
  CM.actionsFromList 
    [ Lock  CT.w1 CT.w2 "secret"         10_000_000
    , Guess CT.w2 CT.w3 "secret" "hello" 3_000_000
    ]

testGuess2 :: Property 
testGuess2 = withMaxSuccess 1 . prop_Game $ 
  CM.actionsFromList 
    [ Lock  CT.w1 CT.w2 "secret"         50_000_000
    , Guess CT.w2 CT.w3 "hello"  "hello" 10_000_000
    , Guess CT.w2 CT.w3 "hello"  "hello" 10_000_000
    , Give  CT.w2 CT.w3
    , Guess CT.w3 CT.w2 "secret" "hello" 10_000_000
    ]

testGuess3 :: Property 
testGuess3 = withMaxSuccess 1 . prop_Game $ 
  CM.actionsFromList 
    [ Lock  CT.w1 CT.w2 "secret"          50_000_000
    , Guess CT.w2 CT.w3 "secret" "hello"  10_000_000
    , Guess CT.w3 CT.w2 "hello" "secret"  10_000_000
    , Guess CT.w2 CT.w3 "wrong" "secret"  10_000_000
    , Give  CT.w2 CT.w3
    , Guess CT.w3 CT.w1 "hello" "secret"  10_000_000
    ]

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

-- Output test results at the end
testsIO :: IO ()
testsIO = quickCheck prop_Game 

-- Output each test and the final results
testsIO' :: IO () 
testsIO' = verboseCheck prop_Game

tests :: TestTree 
tests = testProperty "guess game v2 model" prop_Game
