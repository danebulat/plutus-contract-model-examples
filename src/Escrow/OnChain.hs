{-# LANGUAGE AllowAmbiguousTypes           #-}
{-# LANGUAGE DataKinds                     #-}
{-# LANGUAGE DeriveAnyClass                #-}
{-# LANGUAGE DeriveGeneric                 #-}
{-# LANGUAGE DeriveFunctor                 #-}
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
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE OverloadedStrings             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports                     #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns                   #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context   #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}

module Escrow.OnChain where

import Control.Lens                      (_1, has, makeClassyPrisms, only, review, view)
import Control.Monad                     (void)
import Control.Monad.Error.Lens          (throwing)
import Data.Aeson                        (FromJSON, ToJSON)
import GHC.Generics                      (Generic)

-- On-chain
import PlutusTx                          qualified
import PlutusTx.Prelude                  hiding (pure, (<$>))
import Plutus.V2.Ledger.Api              qualified as LV2
import Plutus.V2.Ledger.Contexts         qualified as LV2Ctx
import Plutus.V1.Ledger.Value            qualified as V
import Plutus.V1.Ledger.Interval         qualified as I
import Prelude                           qualified as P 

import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as V2UtilsTypeScripts
import Plutus.Script.Utils.V2.Typed.Scripts            qualified as Scripts
import Plutus.Script.Utils.V2.Scripts                  (scriptCurrencySymbol)
--
-- Coverage
import PlutusTx.Code                     (getCovIdx)
import PlutusTx.Coverage                 (CoverageIndex)

-- Off-chain
import Ledger                             qualified as L
import Ledger.Ada                         qualified as Ada
import Ledger.Address                     qualified as V1LAddress
import Playground.Contract                (ToSchema)
import Plutus.Contract                    (type (.\/))
import Plutus.Contract                    qualified as PC
import Plutus.Contract.Constraints        qualified as Constraints
import Plutus.Contract.Test.ContractModel qualified as CM

-- ---------------------------------------------------------------------- 
-- Data Types
-- ---------------------------------------------------------------------- 

data EscrowTarget = 
    PaymentPubKeyTarget L.PaymentPubKeyHash V.Value  -- pay to pub key 

PlutusTx.makeLift ''EscrowTarget

-- ---------------------------------------------------------------------- 
-- Parameter
-- ---------------------------------------------------------------------- 

-- | Definition of an escrow contract, consisting of a deadline and a list of targets
data EscrowParams = EscrowParams 
    { escrowDeadline :: LV2.POSIXTime
    -- ^ Latest point at which the outputs may be spent.
    , escrowTargets  :: [EscrowTarget]
    -- ^ Where the money should go. For each target, the contract checks that 
    --   the output `mkTxOutput` of the target is present in the spending tx.
    } 

PlutusTx.makeLift ''EscrowParams

-- ---------------------------------------------------------------------- 
-- Utility Functions
-- ---------------------------------------------------------------------- 

-- | An 'EscrowTarget' that pays the value to a public key address.
payToPubKeyTarget :: L.PaymentPubKeyHash -> V.Value -> EscrowTarget
payToPubKeyTarget = PaymentPubKeyTarget

-- | The total 'Value' that must be paid into the escrow contract before it can be unlocked
targetTotal :: EscrowParams -> V.Value 
targetTotal = P.foldr (\tgt vl -> vl + targetValue tgt) mempty . escrowTargets

-- | The 'Value' specified by an 'EscrowTarget'
targetValue :: EscrowTarget -> V.Value 
targetValue (PaymentPubKeyTarget _ vl) = vl 

-- | Create a 'Ledger.TxOut' value for the target
mkTx :: EscrowTarget -> Constraints.TxConstraints Action L.PaymentPubKeyHash
mkTx (PaymentPubKeyTarget pkh vl) = Constraints.mustPayToPubKey pkh vl 

mkTx' :: V.Value -> L.PaymentPubKeyHash -> EscrowTarget -> Constraints.TxConstraints Action L.PaymentPubKeyHash
mkTx' leftover wpkh (PaymentPubKeyTarget pkh vl)
  | pkh == wpkh = Constraints.mustPayToPubKey pkh (vl <> leftover)
  | otherwise   = Constraints.mustPayToPubKey pkh vl

pubKeyTargetPkh :: EscrowTarget -> L.PaymentPubKeyHash
pubKeyTargetPkh (PaymentPubKeyTarget pkh _) = pkh

-- ---------------------------------------------------------------------- 
-- Redeemer
-- ---------------------------------------------------------------------- 

data Action = Redeem | Refund

PlutusTx.makeIsDataIndexed ''Action [('Redeem, 0), ('Refund, 1)]
PlutusTx.makeLift ''Action

data Escrow 
instance V2UtilsTypeScripts.ValidatorTypes Escrow where 
  type instance RedeemerType Escrow = Action 
  type instance DatumType    Escrow = L.PaymentPubKeyHash

-- ---------------------------------------------------------------------- 
-- Validator Script
-- ---------------------------------------------------------------------- 

-- | @ptx `meetsTarget` tgt@ if @ptx@ pays at least @targetValue tgt@ to the
--   target address.
--
--   The reason why this does not require the target amount to be equal
--   to the actual amount is to enable any excess funds consumed by the
--   spending transaction to be paid to target addresses. This may happen if
--   the target address is also used as a change address for the spending
--   transaction, and allowing the target to be exceeded prevents outsiders from
--   poisoning the contract by adding arbitrary outputs to the script address.

{-# INLINABLE meetsTarget #-}
meetsTarget :: LV2.TxInfo -> EscrowTarget -> Bool 
meetsTarget txInfo (PaymentPubKeyTarget pkh vl) = 
    LV2Ctx.valuePaidTo txInfo (L.unPaymentPubKeyHash pkh) `V.geq` vl

{-# INLINEABLE validate #-}
validate :: EscrowParams -> L.PaymentPubKeyHash -> Action -> LV2.ScriptContext -> Bool 
validate EscrowParams{escrowDeadline, escrowTargets} contributor action 
         LV2.ScriptContext{scriptContextTxInfo} =
  case action of 

    -- Validate tx that pays to escrow targets
    Redeem -> 
         traceIfFalse "escrowDeadline-after" 
          (escrowDeadline `I.after` LV2.txInfoValidRange scriptContextTxInfo)
      && traceIfFalse "meetsTarget" (all (meetsTarget scriptContextTxInfo) escrowTargets)
     
    -- Send funds back to contributor
    Refund -> 
         traceIfFalse "escrowDeadline-before" ((escrowDeadline - 1) `I.before` LV2.txInfoValidRange scriptContextTxInfo)
      && traceIfFalse "txSignedBy" (scriptContextTxInfo `LV2Ctx.txSignedBy` L.unPaymentPubKeyHash contributor)

-- ---------------------------------------------------------------------- 
-- Boilerplate
-- ---------------------------------------------------------------------- 

typedValidator :: EscrowParams -> V2UtilsTypeScripts.TypedValidator Escrow
typedValidator escrow = V2UtilsTypeScripts.mkTypedValidator @Escrow 
    ($$(PlutusTx.compile [|| validate ||]) `PlutusTx.applyCode` PlutusTx.liftCode escrow)
     $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = V2UtilsTypeScripts.mkUntypedValidator

escrowValidator :: EscrowParams -> LV2.Validator 
escrowValidator = Scripts.validatorScript . typedValidator

escrowValidatorHash :: EscrowParams -> LV2.ValidatorHash
escrowValidatorHash = V2UtilsTypeScripts.validatorHash . typedValidator

escrowAddress :: EscrowParams -> L.Address 
escrowAddress = V1LAddress.scriptHashAddress . escrowValidatorHash

-- ---------------------------------------------------------------------- 
-- Coverage
-- ---------------------------------------------------------------------- 

covIdx :: CoverageIndex
covIdx = getCovIdx $$(PlutusTx.compile [|| validate ||])

