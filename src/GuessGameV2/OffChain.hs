{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DerivingStrategies            #-}
{-# LANGUAGE FlexibleContexts              #-}
{-# LANGUAGE ImportQualifiedPost           #-}
{-# LANGUAGE MultiParamTypeClasses         #-}
{-# LANGUAGE NamedFieldPuns                #-}
{-# LANGUAGE NoImplicitPrelude             #-}
{-# LANGUAGE PartialTypeSignatures         #-}
{-# LANGUAGE ScopedTypeVariables           #-}
{-# LANGUAGE TypeApplications              #-}
{-# LANGUAGE TypeFamilies                  #-}
{-# LANGUAGE TypeOperators                 #-}
{-# LANGUAGE OverloadedStrings             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports   #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module GuessGameV2.OffChain where

import Control.Lens                   (_2, (^?))
import Data.Aeson                     (FromJSON, ToJSON)
import Data.ByteString.Char8          qualified as C 
import Data.Map                       (Map)
import Data.Map                       qualified as Map 
import Data.Maybe                     (catMaybes, fromJust)
import Data.Text                      qualified as T
import GHC.Generics                   (Generic)

-- On-chain
import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (pure, (<$>))
import Plutus.V2.Ledger.Api           qualified as LV2
import Plutus.V2.Ledger.Contexts      qualified as LV2Ctx
import Plutus.V1.Ledger.Value         qualified as V
import Prelude                        qualified as P 
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as V2UtilsTypeScripts

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

import GuessGameV2.OnChain            qualified as OnChain

-- ---------------------------------------------------------------------- 
-- Schema
-- ---------------------------------------------------------------------- 

type GameSchema =
        PC.Endpoint "lock"  LockArgs 
    .\/ PC.Endpoint "guess" GuessArgs

-- ---------------------------------------------------------------------- 
-- Data types for contract arguments
-- ---------------------------------------------------------------------- 

-- | Arguments for the @"lock"@ endpoint
data LockArgs = LockArgs 
  { laGameParam  :: !OnChain.GameParam      -- for parameterizing the validator
  , laSecret     :: !P.String               -- initial secret for contract 
  , laValue      :: !LV2.Value              -- value to lock to contract
  , laGuessToken :: !V.AssetClass           -- guess token asset class
  } 
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- | Arguments for the @"guess"@ endpoint
data GuessArgs = GuessArgs 
  { gaGameParam        :: !OnChain.GameParam  -- for parameterizing the validator
  , gaGuessTokenTarget :: !L.Address          -- next recipient to receive token
  , gaOldSecret        :: !P.String           -- the guess
  , gaNewSecret        :: !P.String           -- the new secret
  , gaValueTakenOut    :: !V.Value            -- how much to extract from contract
  } 
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- ====================================================================== 
-- Contract endpoins
-- ====================================================================== 

-- ---------------------------------------------------------------------- 
-- The "lock" contract endpoint 
-- ---------------------------------------------------------------------- 

lock :: PC.Promise () GameSchema T.Text ()
lock = PC.endpoint @"lock" $ \LockArgs{laGameParam, laSecret, laValue, laGuessToken} -> do 
  PC.logInfo @P.String $ "Pay " <> P.show laValue <> " to the script"
  let 
    -- construct datum for contract
    dat = OnChain.Dat { 
            OnChain.datMintingPolicyHash = getMph laGuessToken,
            OnChain.datTokenName         = snd $ V.unAssetClass laGuessToken,
            OnChain.datSecret            = hashString laSecret
          }
  let
    -- construct transaction to produce guessing game
    lookups = Constraints.typedValidatorLookups (OnChain.gameInstance laGameParam)
    tx      = Constraints.mustPayToTheScriptWithDatumInTx dat laValue
  PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx 

-- ---------------------------------------------------------------------- 
-- The "guess" contract endpoint
-- ---------------------------------------------------------------------- 

guess :: PC.Promise () GameSchema T.Text ()
guess = PC.endpoint @"guess" $ \GuessArgs{gaGameParam, gaGuessTokenTarget, gaOldSecret, 
                                          gaNewSecret, gaValueTakenOut} -> do 
  -- Wait for script to have a UTxO of at least 1 lovelace
  PC.logInfo @P.String "Waiting for script to have a UTxO of at least 1 lovelace"
  utxos <- PC.fundsAtAddressGeq (OnChain.gameAddress gaGameParam) (Ada.lovelaceValueOf 1) 
  let 
    -- Get guess token asset class from one of the found utxos
    dat' = fromJust $ getDatum $ head $ Map.toList utxos
    guessToken' = getAssetClass (OnChain.datMintingPolicyHash dat') 
                                (OnChain.datTokenName dat')
  let
    -- Construct redeemer for a MakeGuess action
    redeemer = OnChain.MakeGuess
                 gaGuessTokenTarget          -- next recipient of guess token
                 (clearString gaOldSecret)   -- the guess
                 (hashString gaNewSecret)    -- next secret
                 gaValueTakenOut             -- value to extract from contract
    -- Construct transaction to consume script output
    lookups  = Constraints.typedValidatorLookups (OnChain.gameInstance gaGameParam) P.<>
               Constraints.unspentOutputs utxos
    tx       = Constraints.collectFromTheScript utxos redeemer P.<>
               Constraints.mustPayToAddress gaGuessTokenTarget (V.assetClassValue guessToken' 1)
  -- Submit the transaction
  unbalancedTx' <- PC.mkTxConstraints lookups tx 
  PC.yieldUnbalancedTx unbalancedTx'

-- ---------------------------------------------------------------------- 
-- Top-level contract entry point
-- ---------------------------------------------------------------------- 

contract :: PC.Contract () GameSchema T.Text ()
contract = do 
  PC.logInfo @P.String "Waiting for lock or guess endpoint"
  PC.selectList [lock, guess] >> contract

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

type TxOutTup = (L.TxOutRef, L.DecoratedTxOut)

-- || getDatum
-- Return the datum of a passed in transaction output
getDatum :: TxOutTup -> Maybe OnChain.Dat
getDatum (_, o) = do
    let scriptDat = L._decoratedTxOutScriptDatum o
    case snd scriptDat of
      L.DatumUnknown  -> Nothing
      L.DatumInline d -> PlutusTx.fromBuiltinData (L.getDatum d)
      L.DatumInBody d -> PlutusTx.fromBuiltinData (L.getDatum d)

-- Create a data script for the guessing game by hashing the string 
-- and lifting the hash to its on-chain representation
hashString :: P.String -> OnChain.HashedString
hashString = OnChain.HashedString . sha2_256 . toBuiltin . C.pack

-- Create a redeemer script for the guessing game by lifting the 
-- string to its on-chain representation
clearString :: P.String -> OnChain.ClearString
clearString = OnChain.ClearString . toBuiltin . C.pack

-- | Find the secret word in the Datum of the UTxO
findSecretWordValue :: Map L.TxOutRef LTx.DecoratedTxOut -> Maybe OnChain.HashedString
findSecretWordValue = 
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- | Extract the secret word in the Datum of a given transaction output output
secretWordValue :: LTx.DecoratedTxOut -> Maybe OnChain.HashedString
secretWordValue o = do 
  LV2.Datum d <- o ^? LTx.decoratedTxOutDatum . _2 . LTx.datumInDatumFromQuery 
  PlutusTx.fromBuiltinData d

getMph:: V.AssetClass -> LV2.MintingPolicyHash 
getMph guessToken = 
  LV2.MintingPolicyHash $ LV2.unCurrencySymbol $ fst $ V.unAssetClass guessToken

getAssetClass :: LV2.MintingPolicyHash -> V.TokenName -> V.AssetClass 
getAssetClass (LV2.MintingPolicyHash mph) tn = V.AssetClass (LV2.CurrencySymbol mph, tn)
  
