{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE FlexibleInstances             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE ImportQualifiedPost           #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE NamedFieldPuns                #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE PartialTypeSignatures         #-}
{-# LANGUAGE RecordWildCards               #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TemplateHaskell               #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE ViewPatterns                  #-}
{-# LANGUAGE OverloadedStrings             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-- | A guessing game. A simplified version of 'Plutus.Contract.GameStateMachine'
-- not using 'Plutus.Contract.StateMachine' and using `yieldUnbalancedTx' for
-- balancing, signing and submitting transactions.
--
-- Currently, remote wallets (anything other than WBE) can only handles
-- `yieldUnbalancedTx` requests, and not `balanceTx`, `signTx` and `submitTx`
-- requests.

module GuessGame.Game where

import Control.Lens                   (_2, (^?))
import Data.Aeson                     (FromJSON, ToJSON)
import Data.ByteString.Char8          qualified as C 
import Data.Map                       (Map)
import Data.Map                       qualified as Map 
import Data.Maybe                     (catMaybes)
import Data.Text                      qualified as T
import GHC.Generics                   (Generic)

-- On-chain
import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (pure, (<$>))
import Plutus.V2.Ledger.Api           qualified as LV2
import Plutus.V2.Ledger.Contexts      qualified as LV2Ctx
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as V2UtilsTypeScripts
import Prelude                        qualified as P 

-- Coverage
import PlutusTx.Code                  (getCovIdx)
import PlutusTx.Coverage              (CoverageIndex)

-- Off-chain
import Ledger                         qualified as L
import Ledger.Ada                     qualified as Ada
import Ledger.Address                 qualified as V1LAddress
import Ledger.Constraints             qualified as Constraints 
import Ledger.Typed.Scripts           qualified as Scripts
import Ledger.Tx                      qualified as LTx
import Playground.Contract            (ToSchema)
import Plutus.Contract                (type (.\/))
import Plutus.Contract                qualified as PC 

-- ---------------------------------------------------------------------- 
-- Parameter
-- ---------------------------------------------------------------------- 

data GameParam = GameParam 
  { gpPayPkh    :: L.PaymentPubKeyHash  -- wallet locking funds 
  , gpStartTime :: LV2.POSIXTime         -- starting time of game
  } 
  deriving (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''GameParam
PlutusTx.makeLift ''GameParam

-- ---------------------------------------------------------------------- 
-- Datum
-- ---------------------------------------------------------------------- 

newtype HashedString = HashedString BuiltinByteString
  deriving newtype (P.Show)

PlutusTx.unstableMakeIsData ''HashedString
PlutusTx.makeLift ''HashedString

-- ---------------------------------------------------------------------- 
-- Redeemer
-- ---------------------------------------------------------------------- 

newtype ClearString = ClearString BuiltinByteString
  deriving newtype (P.Show)

PlutusTx.unstableMakeIsData ''ClearString
PlutusTx.makeLift ''ClearString

-- ---------------------------------------------------------------------- 
-- Schema
-- ---------------------------------------------------------------------- 

type GameSchema =
        PC.Endpoint "lock" LockArgs 
    .\/ PC.Endpoint "guess" GuessArgs

data Game
instance Scripts.ValidatorTypes Game where 
  type instance RedeemerType Game = ClearString
  type instance DatumType    Game = HashedString

-- ---------------------------------------------------------------------- 
-- Boilerplate
-- ---------------------------------------------------------------------- 

gameInstance :: GameParam -> V2UtilsTypeScripts.TypedValidator Game 
gameInstance gp = V2UtilsTypeScripts.mkTypedValidator @Game 
  ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode gp)
   $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = V2UtilsTypeScripts.mkUntypedValidator @HashedString @ClearString

-- | The validator script of the game
gameValidator :: GameParam -> LV2.Validator
gameValidator = Scripts.validatorScript . gameInstance

validatorHash' :: GameParam -> LV2.ValidatorHash 
validatorHash' = V2UtilsTypeScripts.validatorHash . gameInstance

gameAddress :: GameParam -> L.Address 
gameAddress = V1LAddress.scriptHashAddress . validatorHash'

-- ---------------------------------------------------------------------- 
-- Validator script
-- ---------------------------------------------------------------------- 

-- | The `GameParam` parameter is not used in the validation. It is 
-- meant to parameterize the script address.

{-# INLINEABLE mkValidator #-}
mkValidator :: GameParam -> HashedString -> ClearString -> LV2Ctx.ScriptContext -> Bool 
mkValidator _ hs cs _ = traceIfFalse "incorrect guess" (isGoodGuess hs cs)

{-# INLINEABLE isGoodGuess #-}
isGoodGuess :: HashedString -> ClearString -> Bool 
isGoodGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'

-- ---------------------------------------------------------------------- 
-- Coverage
-- ---------------------------------------------------------------------- 

-- Doing it this way actually generates coverage locations that we don't care about(!) 
covIdx :: GameParam -> CoverageIndex 
covIdx gameParam = 
  getCovIdx ($$(PlutusTx.compile [|| mkValidator ||]) 
               `PlutusTx.applyCode` PlutusTx.liftCode gameParam)

-- ---------------------------------------------------------------------- 
-- Data types for contract arguments
-- ---------------------------------------------------------------------- 

-- | Arguments for the @"lock"@ endpoint
data LockArgs = LockArgs 
  { laGameParam :: !GameParam 
  , laSecret    :: !P.String
  , laValue     :: !L.Value
  } 
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs = GuessArgs 
  { gaGameParam :: !GameParam
  , gaSecret    :: !P.String 
  } 
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- ======================================================================  
-- Contract endpoins
-- ======================================================================  

-- ---------------------------------------------------------------------- 
-- | The "lock" contract endpoint 
-- ---------------------------------------------------------------------- 

lock :: PC.Promise () GameSchema T.Text ()
lock = PC.endpoint @"lock" $ \LockArgs{laGameParam, laSecret, laValue} -> do 
  PC.logInfo @P.String $ "Pay " <> P.show laValue <> " to the script"
  let
    lookups = Constraints.typedValidatorLookups (gameInstance laGameParam)
    tx      = Constraints.mustPayToTheScriptWithDatumInTx (hashString laSecret) laValue
  PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx 

-- ---------------------------------------------------------------------- 
-- | The "guess" contract endpoint
-- ---------------------------------------------------------------------- 

guess :: PC.Promise () GameSchema T.Text ()
guess = PC.endpoint @"guess" $ \GuessArgs{gaGameParam, gaSecret} -> do 
  -- Wait for script to have a UTxO of at least 1 lovelace
  PC.logInfo @P.String "Waiting for script to have a UTxO of at least 1 lovelace"
  utxos <- PC.fundsAtAddressGeq (gameAddress gaGameParam) (Ada.lovelaceValueOf 1) 
  let 
    redeemer = clearString gaSecret 
    lookups  = Constraints.typedValidatorLookups (gameInstance gaGameParam) P.<>
               Constraints.unspentOutputs utxos
    tx       = Constraints.collectFromTheScript utxos redeemer
  -- Build a transaction that satisfies the constraints
  unbalancedTx' <- PC.mkTxConstraints lookups tx 
  -- Take an UnbalancedTx then balance, sign and submit it to the 
  -- blockchain without returning any results.
  PC.yieldUnbalancedTx unbalancedTx'

-- ---------------------------------------------------------------------- 
-- | Top-level contract entry point
-- ---------------------------------------------------------------------- 

contract :: PC.Contract () GameSchema T.Text ()
contract = do 
  PC.logInfo @P.String "Waiting for lock or guess endpoint"
  PC.selectList [lock, guess] >> contract

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

-- Create a data script for the guessing game by hashing the string 
-- and lifting the hash to its on-chain representation
hashString :: P.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- Create a redeemer script for the guessing game by lifting the 
-- string to its on-chain representation
clearString :: P.String -> ClearString
clearString = ClearString . toBuiltin . C.pack

-- | Find the secret word in the Datum of the UTxO
findSecretWordValue :: Map L.TxOutRef LTx.DecoratedTxOut -> Maybe HashedString
findSecretWordValue = 
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output output
secretWordValue :: LTx.DecoratedTxOut -> Maybe HashedString
secretWordValue o = do 
  LV2.Datum d <- o ^? LTx.decoratedTxOutDatum . _2 . LTx.datumInDatumFromQuery 
  PlutusTx.fromBuiltinData d
