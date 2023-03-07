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

module GuessGameV2.OnChain where

import Data.Aeson                     (FromJSON, ToJSON)
import GHC.Generics                   (Generic)

-- On-chain
import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (pure, (<$>))
import Plutus.V2.Ledger.Api           qualified as LV2
import Plutus.V2.Ledger.Contexts      qualified as LV2Ctx
import Plutus.V1.Ledger.Value         qualified as V
import Prelude                        qualified as P 

import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as V2UtilsTypeScripts
import Plutus.Script.Utils.V2.Typed.Scripts            qualified as Scripts
import Plutus.Script.Utils.V2.Scripts                  (scriptCurrencySymbol)

-- Coverage
import PlutusTx.Code                  (getCovIdx)
import PlutusTx.Coverage              (CoverageIndex)

-- Off-chain
import Ledger                         qualified as L
import Ledger.Ada                     qualified as Ada
import Ledger.Address                 qualified as V1LAddress
import Playground.Contract            (ToSchema)

-- ---------------------------------------------------------------------- 
-- Parameter
-- ---------------------------------------------------------------------- 

data GameParam = GameParam 
  { gpCreator   :: L.Address      -- wallet locking funds 
  , gpStartTime :: LV2.POSIXTime  -- starting time of game
  } 
  deriving stock (P.Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToSchema)

PlutusTx.unstableMakeIsData ''GameParam
PlutusTx.makeLift ''GameParam

-- ---------------------------------------------------------------------- 
-- Datum
-- ---------------------------------------------------------------------- 

newtype HashedString = HashedString BuiltinByteString
  deriving newtype (P.Eq, P.Show)

instance Eq HashedString where 
  HashedString s == HashedString s' = s == s'

PlutusTx.makeIsDataIndexed ''HashedString [('HashedString, 0)]
PlutusTx.makeLift ''HashedString

data Dat = Dat 
  { datMintingPolicyHash :: LV2.MintingPolicyHash  -- guess token policy hash
  , datTokenName         :: LV2.TokenName          -- guess token name
  , datSecret            :: HashedString           -- hash of secret
  } deriving stock (P.Eq, P.Show)

instance Eq Dat where 
  Dat mph tn s == Dat mph' tn' s' = 
    mph == mph' && tn == tn' && s == s'

PlutusTx.makeIsDataIndexed ''Dat [('Dat, 0)]

-- ---------------------------------------------------------------------- 
-- Redeemer
-- ---------------------------------------------------------------------- 

newtype ClearString = ClearString BuiltinByteString
  deriving newtype (P.Show)

PlutusTx.makeIsDataIndexed ''ClearString [('ClearString, 0)]
PlutusTx.makeLift ''ClearString

data Red 
  = MintToken 
  | MakeGuess 
      L.Address     -- guess token recipient 
      ClearString   -- guess 
      HashedString  -- hash of new secret word
      L.Value       -- value to unlock from script utxo

PlutusTx.makeIsDataIndexed ''Red [('MintToken, 0), ('MakeGuess, 1)]

-- ---------------------------------------------------------------------- 
-- Validator types 
-- ---------------------------------------------------------------------- 

data Game
instance Scripts.ValidatorTypes Game where 
  type instance RedeemerType Game = Red
  type instance DatumType    Game = Dat

-- ---------------------------------------------------------------------- 
-- Validator script
-- ---------------------------------------------------------------------- 

{-# INLINABLE minLovelace #-}
minLovelace :: V.Value
minLovelace = Ada.lovelaceValueOf (Ada.getLovelace  L.minAdaTxOut)

{-# INLINABLE getUtxoDatum #-}
getUtxoDatum :: LV2.TxOut -> Dat 
getUtxoDatum out = case LV2.txOutDatum out of
  LV2.NoOutputDatum     -> traceError "no datum in own output"
  LV2.OutputDatumHash _ -> traceError "datum hash not expected"
  LV2.OutputDatum d     ->
    case PlutusTx.fromBuiltinData $ LV2.getDatum d of
      Nothing -> traceError "error converting to datum"
      Just dat -> dat

-- | The `GameParam` parameter is not used in the validation. It is 
-- meant to parameterize the script address.

{-# INLINEABLE mkValidator #-}
mkValidator :: GameParam -> Dat -> Red -> LV2.ScriptContext -> Bool 
mkValidator _ dat red ctx = case red of 
  -- MintToken -> 
  --   traceIfFalse "must have one script output" checkNumOutputs &&
  --   traceIfFalse "no value being locked"       checkOutputValueNonZero

  MakeGuess recipient guess newSecret val -> 
    traceIfFalse "guess token not in output"   (checkRecipientHasToken recipient) &&
    traceIfFalse "wrong guess"                 (checkGuess guess $ datSecret dat) &&
    traceIfFalse "wrong output value"          (checkOutputVal val)               &&
    traceIfFalse "wrong output datum"          (checkOutputDatum newSecret)       &&
    traceIfFalse "must have one script output" checkNumOutputs
  where 
    txInfo :: LV2.TxInfo 
    txInfo = LV2.scriptContextTxInfo ctx

    getOwnOutput :: LV2.TxOut
    getOwnOutput = case LV2Ctx.getContinuingOutputs ctx of 
      [o] -> o 
      _   -> traceError "expected one script output"

    checkNumOutputs :: Bool 
    checkNumOutputs = length [getOwnOutput] == 1

    -- checkOutputValueNonZero :: Bool 
    -- checkOutputValueNonZero = V.isZero (LV2.txOutValue getOwnOutput)

    checkGuess :: ClearString -> HashedString -> Bool 
    checkGuess (ClearString guess') (HashedString secret) = 
      sha2_256 guess' == secret

    getOutputPaidToRecipient :: L.Address -> V.Value
    getOutputPaidToRecipient r = 
       case filter ((== r) . LV2.txOutAddress) $ LV2.txInfoOutputs txInfo of
        [] -> traceError "invalid recipient output"
        xs -> foldr (\o acc -> acc <> LV2.txOutValue o) (Ada.lovelaceValueOf 0) xs

    checkRecipientHasToken :: L.Address -> Bool 
    checkRecipientHasToken r = (== 1) $ V.assetClassValueOf val
      (V.AssetClass (curSym, datTokenName dat))
      where 
       curSym = mintingPolicyHashToCurySym (datMintingPolicyHash dat)
       val    = getOutputPaidToRecipient r

    mintingPolicyHashToCurySym :: LV2.MintingPolicyHash -> LV2.CurrencySymbol 
    mintingPolicyHashToCurySym (LV2.MintingPolicyHash mph) =
      LV2.CurrencySymbol mph

    checkOutputVal :: V.Value -> Bool 
    checkOutputVal val = case LV2Ctx.findOwnInput ctx of 
      Nothing -> traceError "own input not found"
      Just txInInfo -> 
        LV2.txOutValue (LV2.txInInfoResolved txInInfo) <> negate val <> minLovelace == 
        LV2.txOutValue getOwnOutput

    checkOutputDatum :: HashedString -> Bool 
    checkOutputDatum hs = dat{datSecret = hs} == getUtxoDatum getOwnOutput

-- ---------------------------------------------------------------------- 
-- Boilerplate
-- ---------------------------------------------------------------------- 

gameInstance :: GameParam -> V2UtilsTypeScripts.TypedValidator Game 
gameInstance = V2UtilsTypeScripts.mkTypedValidatorParam @Game 
  $$(PlutusTx.compile [|| mkValidator ||])
  $$(PlutusTx.compile [|| wrap ||])
  where 
    wrap = V2UtilsTypeScripts.mkUntypedValidator @Dat @Red

-- | The validator script of the game
gameValidator :: GameParam -> LV2.Validator
gameValidator = Scripts.validatorScript . gameInstance

validatorHash' :: GameParam -> LV2.ValidatorHash 
validatorHash' = V2UtilsTypeScripts.validatorHash . gameInstance

-- | The address of the game (hash of its validator script)
gameAddress :: GameParam -> L.Address
gameAddress = V1LAddress.scriptHashAddress . validatorHash'

-- ---------------------------------------------------------------------- 
-- Coverage
-- ---------------------------------------------------------------------- 

-- Doing it this way actually generates coverage locations that we
-- don't care about(!) 
covIdx :: GameParam -> CoverageIndex 
covIdx gameParam = 
  getCovIdx ($$(PlutusTx.compile [|| mkValidator ||]) 
               `PlutusTx.applyCode` PlutusTx.liftCode gameParam)

-- ---------------------------------------------------------------------- 
-- Free policy script
-- ---------------------------------------------------------------------- 

{-# INLINEABLE mkFreePolicy #-}
mkFreePolicy :: () -> LV2Ctx.ScriptContext -> Bool
mkFreePolicy () _ = True

freePolicy :: Scripts.MintingPolicy
freePolicy = LV2.mkMintingPolicyScript
  $$(PlutusTx.compile [|| Scripts.mkUntypedMintingPolicy mkFreePolicy ||])

freeCurSymbol :: LV2.CurrencySymbol
freeCurSymbol = scriptCurrencySymbol freePolicy

-- ---------------------------------------------------------------------- 
-- NFT policy script
-- ---------------------------------------------------------------------- 

{-# INLINEABLE mkNftPolicy #-}
mkNftPolicy :: LV2.TxOutRef -> LV2.TokenName -> () -> LV2Ctx.ScriptContext -> Bool 
mkNftPolicy oref tn () ctx = 
  traceIfFalse "utxo not consumed"   hasUtxo &&
  traceIfFalse "wrong amount minted" checkMintedAmount
  where
    txInfo :: LV2.TxInfo 
    txInfo = LV2.scriptContextTxInfo ctx 

    hasUtxo :: Bool 
    hasUtxo = any (\i -> LV2.txInInfoOutRef i == oref) (LV2.txInfoInputs txInfo)

    checkMintedAmount :: Bool 
    checkMintedAmount = case V.flattenValue (LV2.txInfoMint txInfo) of 
      [(_, tn', amt')] -> tn' == tn && amt' == 1 
      _ -> False
 
nftPolicy :: LV2.TxOutRef -> LV2.TokenName -> LV2.MintingPolicy 
nftPolicy oref tn = LV2.mkMintingPolicyScript $
  $$(PlutusTx.compile [|| \oref' tn' -> Scripts.mkUntypedMintingPolicy (mkNftPolicy oref' tn') ||])
  `PlutusTx.applyCode` PlutusTx.liftCode oref 
  `PlutusTx.applyCode` PlutusTx.liftCode tn

nftCurSymbol :: LV2.TxOutRef -> LV2.TokenName -> LV2.CurrencySymbol
nftCurSymbol oref tn = scriptCurrencySymbol $ nftPolicy oref tn 

nftPolicyScript :: LV2.TxOutRef -> LV2.TokenName -> LV2.Script 
nftPolicyScript oref tn = LV2.unMintingPolicyScript $ nftPolicy oref tn

nftMintValidator :: LV2.TxOutRef -> LV2.TokenName -> LV2.Validator
nftMintValidator oref tn = LV2.Validator $ nftPolicyScript oref tn
