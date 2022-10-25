{-  HLINT ignore "Redundant <$>"    -}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE OverloadedStrings   #-}

{-|
Module      : Escrow.OffChain
Description : OffChain code for Escrow Contract (congestive version).
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define here the interface for accessing the contract functions through
an Schema. The main offchain functions implement all the logic for
submitting transactions to the blockchain.
-}

module Escrow.OffChain where

import qualified Data.Map                  as Map
import           Data.Text
import           Data.Monoid       (Last (..))
import           Control.Monad             (forever)
import           Control.Lens

import           PlutusTx.Numeric          as PNum
import           Plutus.Contract           as Contract
import           Ledger
import           Ledger.Constraints        as Constraints
import           Ledger.Value              as Value
import           Ledger.Ada

import           Utils.Currency            as Currency
import           Utils.OffChain

import           Escrow.Business
import           Escrow.Types
import           Escrow.Validator

-- | Contract Schema
type EscrowSchema = Endpoint "addPayment" (PaymentPubKeyHash,Integer)
                .\/ Endpoint "collect" PaymentPubKeyHash

-- | Initialization of the contract. It creates the parameter
--   and submits the first utxo containing the initial state.
run :: TxOutRef -> Contract (Last Parameter) EmptySchema Text ()
run txRef = start txRef >>= tell . pure

endpoints
    :: Parameter
    -> Contract () EscrowSchema Text ()
endpoints p = forever $ handleError logError $ awaitPromise $
              addPaymentEp `select` collectEp
  where
    addPaymentEp :: Promise () EscrowSchema Text ()
    addPaymentEp = endpoint @"addPayment" $ addPaymentOp p

    collectEp :: Promise () EscrowSchema Text ()
    collectEp = endpoint @"collect" $ collectOp p

-- | Mints the contract NFT and produces the first utxo containing the
--   initial state. Returns the contract parameter.
start
    :: forall w s
    .  TxOutRef
    -> Contract w s Text Parameter
start txRef = do
    (nftContract, nftLkp, nftTx) <- mintCurrencyWithUTxO @OneShot
                                                         [(escrowTokenName,1)]
                                                         txRef
    logInfo @String "Starting contract"
    logInfo @String $ "Contract NFT: " ++ show nftContract

    let cs            = Currency.curSymbol nftContract
        contractAsset = AssetClass (cs, escrowTokenName)
        nftV          = assetClassValue contractAsset 1
        parameter     = mkParameter contractAsset
        val           = minAda <> nftV
        datum         = mkEscrowDatum initialEscrowState

        lkp = mconcat
              [ Constraints.typedValidatorLookups (escrowInst parameter)
              , Constraints.plutusV1OtherScript (escrowValidator parameter)
              , nftLkp
              ]
        tx  = mconcat
              [ Constraints.mustPayToTheScriptWithDatumInTx datum val
              , nftTx
              ]

    handleTxConstraints @Escrowing lkp tx
    logInfo @String $ "Addr: " ++ show (escrowAddress parameter)
    logInfo @String "Contract started"

    return parameter

-- | Updates the state of the contract by adding a payment to it.
addPaymentOp
    :: forall w s
    .  Parameter
    -> (PaymentPubKeyHash, Integer)
    -> Contract w s Text ()
addPaymentOp p (pkh, m) = do
    (oref,outxo) <- lookupScriptUtxo (escrowAddress p) (stateNFT p)
    logInfo @String $ "Ble" <> show outxo
    d <- datumFromHash $ fst $ _ciTxOutScriptDatum outxo
    logInfo @String $ "Ble" <> show d
    datum        <- getContractDatum outxo

    let newState = addPayment (eState datum) pkh m in
        case newState of
            Nothing -> logError @String
                       "Payment operation failed: tried to pay with amount <=0"
            Just st -> do
                let upDatum     = mkEscrowDatum st
                    scriptValue = outxo ^. ciTxOutValue <> lovelaceValueOf m

                    lkp = contractLookups p [(oref,outxo)]
                    tx  = mconcat
                          [ Constraints.mustSpendScriptOutput oref (addPayRedeemer pkh m)
                          , Constraints.mustPayToTheScriptWithDatumInTx upDatum scriptValue
                          ]

                handleTxConstraints @Escrowing lkp tx
                logInfo @String "Payment operation submitted successfully"

-- | Collects the amount specified in the state for the signer's public key, if
--   it exists.
collectOp
    :: forall w s
    .  Parameter
    -> PaymentPubKeyHash
    -> Contract w s Text ()
collectOp p pkh = do
    (oref,outxo) <- lookupScriptUtxo (escrowAddress p) (stateNFT p)
    datum        <- getContractDatum outxo

    case collect (eState datum) pkh of
        Nothing -> logError @String
                   "Collect operation failed: signer not in state"
        Just (st,val) -> do
            let upDatum     = mkEscrowDatum st
                scriptValue = outxo ^. ciTxOutValue PNum.- lovelaceValueOf val

                lkp = contractLookups p [(oref,outxo)]
                tx  = mconcat
                      [ Constraints.mustSpendScriptOutput oref collectRedeemer
                      , Constraints.mustPayToTheScriptWithDatumInTx upDatum scriptValue
                      , Constraints.mustBeSignedBy pkh
                      ]

            handleTxConstraints @Escrowing lkp tx
            logInfo @String "Collect operation submitted successfully"

-- | Lookups for submitting a transaction spending some utxos.
contractLookups
    :: Parameter
    -> [(TxOutRef,ChainIndexTxOut)]
    -> ScriptLookups Escrowing
contractLookups p utxos = mconcat
    [ Constraints.unspentOutputs (Map.fromList utxos)
    , Constraints.typedValidatorLookups (escrowInst p)
    , Constraints.plutusV1OtherScript (escrowValidator p)
    ]

-- | Lifted function for getting datum from a ChainIndexTxOut.
getContractDatum
    :: forall w s
    .  ChainIndexTxOut
    -> Contract w s Text EscrowDatum
getContractDatum = maybe (throwError "Can't find contract datum") return .
                   getChainIndexTxOutDatum
