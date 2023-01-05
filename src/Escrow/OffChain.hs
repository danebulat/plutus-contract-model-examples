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
{-# LANGUAGE GeneralizedNewtypeDeriving    #-}
{-# LANGUAGE UndecidableInstances          #-}

{-# OPTIONS_GHC -fno-warn-unused-imports                     #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns                   #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:debug-context   #-}
{-# OPTIONS_GHC -g -fplugin-opt PlutusTx.Plugin:coverage-all #-}

module Escrow.OffChain where

import Control.Lens                   (_1, has, makeClassyPrisms, only, review, view, (^.))
import Control.Monad                  (void, when)
import Control.Monad.Error.Lens       (throwing)
import Data.Aeson                     (FromJSON, ToJSON)
import Data.Map                       qualified as Map
import Data.Text                      qualified as T
import GHC.Generics                   (Generic)

import PlutusTx                       qualified
import PlutusTx.Prelude               hiding (pure, (<$>))
import Plutus.V2.Ledger.Api           qualified as LV2
import Plutus.V2.Ledger.Contexts      qualified as LV2Ctx
import Plutus.V1.Ledger.Interval      qualified as I
import Plutus.V1.Ledger.Value         qualified as V
import Prelude                        qualified as P 
import Plutus.Script.Utils.V2.Typed.Scripts.Validators qualified as V2UtilsTypeScripts

import Ledger                         qualified as L
import Ledger.Ada                     qualified as Ada
import Ledger.Address                 qualified as V1LAddress
import Ledger.Constraints             qualified as Constraints 
import Ledger.Typed.Scripts           qualified as Scripts
import Ledger.Tx                      qualified as LTx
import Playground.Contract            (ToSchema)
import Plutus.Contract                (type (.\/), endpoint)
import Plutus.Contract                qualified as PC 
import Plutus.Contract.Error

import Escrow.OnChain                 qualified as OnChain
import Escrow.OnChain                 (EscrowParams, targetTotal, mkTx, escrowDeadline)

-- ---------------------------------------------------------------------- 
-- Schema
-- ---------------------------------------------------------------------- 

type EscrowSchema = 
      PC.Endpoint "pay-escrow" V.Value   -- pay to escrow script address
  .\/ PC.Endpoint "redeem-escrow" ()     -- pay to beneficiaries
  .\/ PC.Endpoint "refund-escrow" ()     -- refund a contributor

-- ---------------------------------------------------------------------- 
-- Top-level Contract
-- ---------------------------------------------------------------------- 

escrowContract 
    :: (PC.AsContractError e) 
    => EscrowParams 
    -> PC.Contract () EscrowSchema e ()
escrowContract escrowParams = 
  let inst = OnChain.typedValidator escrowParams
      payAndRefund = endpoint @"pay-escrow" $ \vl -> do 
        _ <- pay inst escrowParams vl 
        _ <- PC.awaitTime $ escrowDeadline escrowParams
        refund inst escrowParams
  in PC.selectList
      [ void payAndRefund 
      , void $ redeemEp escrowParams  -- @"redeem-escrow"
      ]

-- ---------------------------------------------------------------------- 
-- Pay Endpoint
-- ---------------------------------------------------------------------- 

-- Endpoint that gets the owner's public key and the contribution.
payEp 
    :: forall w s e. 
       (PC.AsContractError e, PC.HasEndpoint "pay-escrow" V.Value s)
    => EscrowParams
    -> PC.Promise w s e LV2.TxId
payEp escrowParams = 
  endpoint @"pay-escrow" $ pay (OnChain.typedValidator escrowParams) escrowParams

-- Pay some money to the escrow contract.
pay 
    :: forall w s e. (PC.AsContractError e)
    => V2UtilsTypeScripts.TypedValidator OnChain.Escrow  -- The instance
    -> EscrowParams                                      -- The escrow contract params
    -> V.Value                                           -- How much money to pay in 
    -> PC.Contract w s e LV2.TxId                        -- Submitted tx id
pay inst escrowParams vl = do 
  pk <- PC.ownFirstPaymentPubKeyHash 
  let tx = Constraints.mustPayToTheScriptWithDatumInTx pk vl
        <> Constraints.mustValidateIn (I.interval 1 (OnChain.escrowDeadline escrowParams))

  -- Build a tx that satisfies the constraints
  PC.mkTxConstraints (Constraints.typedValidatorLookups inst) tx
    >>= PC.adjustUnbalancedTx 
    >>= PC.submitUnbalancedTx 
    >>= return . L.getCardanoTxId

-- ---------------------------------------------------------------------- 
-- Redeem Endpoint
-- ---------------------------------------------------------------------- 

redeemEp 
    :: forall w s e. 
    (PC.AsContractError e, PC.HasEndpoint "redeem-escrow" () s)
    => EscrowParams 
    -> PC.Promise w s e () 
redeemEp escrowParams = PC.endpoint @"redeem-escrow" $ \() -> 
  let inst = OnChain.typedValidator escrowParams
  in redeem inst escrowParams

-- | Redeem all outputs at the contract address using a transaction that 
--   has all the outputs defined in the contract's list of targets.
redeem 
    :: forall w s e. PC.AsContractError e 
    => V2UtilsTypeScripts.TypedValidator OnChain.Escrow 
    -> EscrowParams
    -> PC.Contract w s e () 
redeem inst escrowParams = do 
  -- Get script address and current time
  unspentOutputs <- PC.utxosAt (OnChain.escrowAddress escrowParams) 
  current <- snd P.<$> PC.currentNodeClientTimeRange
  
  -- Check deadline hasn't passed
  let deadlinePassed = current >= OnChain.escrowDeadline escrowParams
  when deadlinePassed $ do 
    PC.logError @P.String "Deadline Passed" 

  -- Continue if deadline not passed
  when (not deadlinePassed) $ do 
    let outputList = Map.elems unspentOutputs
        totalVal   = foldr (\o acc -> o ^. LTx.decoratedTxOutValue <> acc) mempty outputList
        
    -- Check enough funds are at the address
    let insufficientFunds = totalVal `V.lt` targetTotal escrowParams
    when insufficientFunds $ do
      PC.logError @P.String "Not Enough Funds At Address" 

    -- Continue if script address holds sufficient funds
    when (not insufficientFunds) $ do
      let 
        -- Note: Minus 1 necessary for on-script validation to pass
        validityTimeRange = I.to $ OnChain.escrowDeadline escrowParams - 1
        tx = Constraints.collectFromTheScript unspentOutputs OnChain.Redeem
          <> foldMap mkTx (OnChain.escrowTargets escrowParams) 
          <> Constraints.mustValidateIn validityTimeRange
      
      utx <- PC.mkTxConstraints 
               (Constraints.typedValidatorLookups inst P.<>
                Constraints.unspentOutputs unspentOutputs) tx
      adjusted <- PC.adjustUnbalancedTx utx
      ledgerTx <- PC.submitUnbalancedTx adjusted
      PC.logInfo $ "Submitted Tx: " ++ P.show ledgerTx

-- ---------------------------------------------------------------------- 
-- Refund Endpoint 
-- ---------------------------------------------------------------------- 

refundEp
    :: forall w s e. 
       (PC.AsContractError e, PC.HasEndpoint "refund-escrow" () s)
    => EscrowParams
    -> PC.Promise w s e () 
refundEp escrowParams = PC.endpoint @"refund-escrow" $ \() -> 
  let inst = OnChain.typedValidator escrowParams
  in refund inst escrowParams

-- Claim a refund of the contribution
refund
    :: forall w s e. (PC.AsContractError e) => 
       V2UtilsTypeScripts.TypedValidator OnChain.Escrow 
    -> EscrowParams
    -> PC.Contract w s e () 
refund inst escrow = do 
  -- Get script outputs and pkh
  unspentOutputs <- PC.utxosAt (OnChain.escrowAddress escrow) 
  pk <- PC.ownFirstPaymentPubKeyHash 

  -- Construct datum from wallet's pkh
  let pkh = L.datumHash $ LV2.Datum $ PlutusTx.toBuiltinData pk 

  -- Filter script outputs that contain the pkh in the datum
  let flt _ ciTxOut = has (LTx.decoratedTxOutScriptDatum . _1 . only pkh) ciTxOut 
      tx' = Constraints.collectFromTheScriptFilter flt unspentOutputs OnChain.Refund
         <> Constraints.mustBeSignedBy pk 
         <> Constraints.mustValidateIn (L.from $ OnChain.escrowDeadline escrow)

  -- Submit transaction if it modifies the UTXO set
  let txModifiesUtxoSet = Constraints.modifiesUtxoSet tx'
  when txModifiesUtxoSet $ do 
    let lookups = Constraints.typedValidatorLookups inst P.<>
                  Constraints.unspentOutputs unspentOutputs
    utx      <- PC.mkTxConstraints lookups tx'
    adjusted <- PC.adjustUnbalancedTx utx 
    ledgerTx <- PC.submitUnbalancedTx adjusted
    PC.logInfo @P.String $ "Submitted Tx: " ++ P.show ledgerTx
  
  -- Log error when transaction doesn't modify the UTXO set
  when (not txModifiesUtxoSet) $ do
    PC.logError @P.String "Refund Failed"

-- ---------------------------------------------------------------------- 
-- Contract that calls all endpoints
-- ---------------------------------------------------------------------- 

-- | Pay some money into the escrow contract. Then release all funds to their
--   specified targets if enough funds were deposited before the deadline,
--   or reclaim the contribution if the goal has not been met.
payRedeemRefund
    :: forall w s e. (PC.AsContractError e) 
    => EscrowParams
    -> V.Value 
    -> PC.Contract w s e ()
payRedeemRefund params vl = do 
  let inst = OnChain.typedValidator params 
      go = do 
        cur <- PC.utxosAt (OnChain.escrowAddress params)
        let
          outputs    = Map.elems cur
          presentVal = foldr (\o acc -> o ^. LTx.decoratedTxOutValue <> acc) mempty outputs
        if presentVal `V.geq` targetTotal params 
          then redeem inst params 
          else do 
            time <- snd P.<$> PC.currentNodeClientTimeRange 
            if time >= OnChain.escrowDeadline params 
              then refund inst params 
              else PC.waitNSlots 1 >> go
  
  -- Pay the value `vl` to the contract
  _ <- pay inst params vl
  go

