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

import Control.Monad                  (void)
import Control.Lens                   (_2, (^?))
import Data.Aeson                     (FromJSON, ToJSON)
import Data.ByteString.Char8          qualified as C 
import Data.Map                       (Map)
import Data.Map                       qualified as Map 
import Data.Maybe                     (catMaybes, fromJust)
import Data.Text                      qualified as T
import GHC.Generics                   (Generic)
import Text.Printf                    (printf)

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
    .\/ PC.Endpoint "give"  GiveArgs

-- ---------------------------------------------------------------------- 
-- Data types for contract arguments
-- ---------------------------------------------------------------------- 

-- | Arguments for the @"lock"@ endpoint
data LockArgs = LockArgs 
  { laGameParam  :: !OnChain.GameParam        -- for parameterizing the validator
  , laSecret     :: !P.String                 -- initial secret for contract 
  , laValue      :: !LV2.Value                -- value to lock to contract
  , laGuessToken :: !V.AssetClass             -- guess token asset class
  , laRecipient  :: !L.Address                -- first recipient of guess token
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

-- | Arguments for the @"give" endpoint
data GiveArgs = GiveArgs 
  { gvRecipient  :: !L.Address                  -- recipient of guess token
  , gvGuessToken :: !V.AssetClass 
  }
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

-- ====================================================================== 
-- Contract endpoins
-- ====================================================================== 

-- ---------------------------------------------------------------------- 
-- The "give" contract endpoint 
-- ---------------------------------------------------------------------- 

give :: PC.Promise () GameSchema T.Text () 
give = PC.endpoint @"give" $ \GiveArgs{gvRecipient, gvGuessToken} -> do 
  PC.logInfo @P.String $ printf "Sending guess token to %s" (P.show gvRecipient)
  let 
    val = V.assetClassValue gvGuessToken 1 P.<> minLovelace
    tx  = Constraints.mustPayToAddress gvRecipient val
  ledgerTx <- PC.submitTx tx
  void $ PC.awaitTxConfirmed (L.getCardanoTxId ledgerTx)

-- ---------------------------------------------------------------------- 
-- The "lock" contract endpoint 
-- ---------------------------------------------------------------------- 

lock :: PC.Promise () GameSchema T.Text ()
lock = PC.endpoint @"lock" $ \LockArgs{laGameParam, laSecret, laValue, laGuessToken, laRecipient} -> do 
  PC.logInfo @P.String $ "Pay " <> P.show laValue <> " to the script"
  let 
    mph = getMph laGuessToken
    tn  = snd $ V.unAssetClass laGuessToken
    val = V.singleton OnChain.freeCurSymbol tn 1

    -- Construct datum for contract
    dat = OnChain.Dat { 
            OnChain.datMintingPolicyHash = mph,
            OnChain.datTokenName         = tn,
            OnChain.datSecret            = hashString laSecret
          }
  let
    -- Construct transaction to produce script utxo and mint guess token
    lookups = Constraints.typedValidatorLookups (OnChain.gameInstance laGameParam) P.<> 
              Constraints.plutusV2MintingPolicy OnChain.freePolicy 
    
    tx      = Constraints.mustPayToTheScriptWithInlineDatum dat laValue P.<>
              Constraints.mustMintValue val P.<>
              Constraints.mustPayToAddress laRecipient 
                (V.assetClassValue laGuessToken 1 <> minLovelace) 

  PC.mkTxConstraints lookups tx >>= PC.adjustUnbalancedTx >>= PC.yieldUnbalancedTx 

-- ---------------------------------------------------------------------- 
-- The "guess" contract endpoint
-- ---------------------------------------------------------------------- 

guess :: PC.Promise () GameSchema T.Text ()
guess = PC.endpoint @"guess" $ \GuessArgs{gaGameParam, gaGuessTokenTarget, gaOldSecret, 
                                          gaNewSecret, gaValueTakenOut} -> do 
  pkh <- PC.ownFirstPaymentPubKeyHash
  PC.logInfo @P.String "Waiting for script to have a UTxO of at least 1 lovelace"
  utxos <- PC.fundsAtAddressGeq (OnChain.gameAddress gaGameParam) (Ada.lovelaceValueOf 1) 
  let 
    -- Get guess token asset class from the found script utxo
    (oref, o) = head $ Map.toList utxos 
    dat       = fromJust $ getDatum (oref, o) 
    val       = L._decoratedTxOutValue o
    guessTn   = getAssetClass dat

    -- Construct redeemer for a MakeGuess action
    redeemer = OnChain.MakeGuess
                 gaGuessTokenTarget          -- next recipient of guess token
                 (clearString gaOldSecret)   -- the guess
                 (hashString gaNewSecret)    -- next secret
                 gaValueTakenOut             -- value to extract from contract

    -- Construct new datum
    newDatum = dat{OnChain.datSecret = hashString gaNewSecret}
     
    -- Construct transaction to consume script output
    lookups  = Constraints.typedValidatorLookups (OnChain.gameInstance gaGameParam) P.<>
               Constraints.unspentOutputs (Map.singleton oref o)
    tx       = -- Spend found script utxo 
               Constraints.mustSpendScriptOutput oref 
                 (L.Redeemer $ PlutusTx.toBuiltinData redeemer)       P.<>
               -- Send new datum and calculate new script value 
               Constraints.mustPayToTheScriptWithInlineDatum newDatum 
                (minLovelace P.<> val P.<> negate gaValueTakenOut)    P.<>
               -- Receive funds from script to this wallet
               Constraints.mustPayToPubKey pkh gaValueTakenOut        P.<>
               -- Send guess token to new recipient
               Constraints.mustPayToAddress gaGuessTokenTarget 
                (V.assetClassValue guessTn 1 P.<> minLovelace)

  -- Submit the transaction
  unbalancedTx' <- PC.mkTxConstraints lookups tx 
  PC.yieldUnbalancedTx unbalancedTx'

-- ---------------------------------------------------------------------- 
-- Top-level contract entry point
-- ---------------------------------------------------------------------- 

contract :: PC.Contract () GameSchema T.Text ()
contract = do 
  PC.logInfo @P.String "Waiting for lock or guess endpoint"
  PC.selectList [lock, guess, give] >> contract

-- ---------------------------------------------------------------------- 
-- Helper functions
-- ---------------------------------------------------------------------- 

type TxOutTup = (L.TxOutRef, L.DecoratedTxOut)

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

-- Find the secret word in the Datum of the UTxO
findSecretWordValue :: Map L.TxOutRef LTx.DecoratedTxOut -> Maybe OnChain.HashedString
findSecretWordValue = 
  listToMaybe . catMaybes . Map.elems . Map.map secretWordValue

-- Extract the secret word in the Datum of a given transaction output output
secretWordValue :: LTx.DecoratedTxOut -> Maybe OnChain.HashedString
secretWordValue o = do 
  LV2.Datum d <- o ^? LTx.decoratedTxOutDatum . _2 . LTx.datumInDatumFromQuery 
  PlutusTx.fromBuiltinData d

getMph:: V.AssetClass -> LV2.MintingPolicyHash 
getMph guessToken = 
  LV2.MintingPolicyHash $ LV2.unCurrencySymbol $ fst $ V.unAssetClass guessToken

getAssetClass :: OnChain.Dat -> V.AssetClass 
getAssetClass d = V.AssetClass (LV2.CurrencySymbol mph, OnChain.datTokenName d)
  where LV2.MintingPolicyHash mph = OnChain.datMintingPolicyHash d 

minLovelace :: V.Value
minLovelace = Ada.lovelaceValueOf (Ada.getLovelace  L.minAdaTxOut)

