{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Escrow.OnChain
Description : OnChain validator for the Escrow contract (congestive version).
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define here only the contract validator.
-}

module Escrow.OnChain (mkEscrowValidator) where

-- Third-party libraries.
import           Ledger
import           Ledger.Ada
import           PlutusTx.Numeric              as PNum
import           PlutusTx.Prelude

-- Internal modules.
import           Escrow.Business
import           Escrow.Types
import           Utils.OnChain

{-# INLINABLE mkEscrowValidator #-}
mkEscrowValidator
    :: Parameter -> EscrowDatum -> EscrowRedeemer -> ScriptContext -> Bool
mkEscrowValidator p (EscrowDatum st) (AddPayRedeemer pkh m) ctx =
    validateAddPay p st pkh m ctx
mkEscrowValidator p (EscrowDatum st) CollectRedeemer ctx =
    validateCollect p st ctx

-- | Validation for 'addPayment' operation when the Escrow UTxO is spent.
--   In the following case, from the standpoint of the Escrow script UTxO, we
--   must check that:
--     * the NFT is in the output UTxO,
--     * the Datum in the output UTxO is the same as applying the business
--       logic to the input UTxO,
--     * the Value in the output UTxO is the same as the inputValue combined
--       with the amount passed as argument,
--     * we are not minting any tokens.
{-# INLINABLE validateAddPay #-}
validateAddPay
    :: Parameter
    -> EscrowState
    -> PaymentPubKeyHash
    -> Integer
    -> ScriptContext
    -> Bool
validateAddPay p st pkh m ctx =
       debug "checkAddPayment: NFT missing in the output UTxO."
           (outputHasNFT (stateNFT p) ctx)
    && debug "checkAddPayment: Output UTxO datum is wrong." checkDatum
    && debug "checkAddPayment: Amount paid is wrong."       checkValue
    && debug "checkAddPayment: Tokens are minted."          checkMinting
  where
    input, output :: TxOut
    input = case findOwnInput ctx of
        Just txInInfo -> txInInfoResolved txInInfo
        _             -> traceError "checkAddPayment: Input UTxO not found."
    output = case getContinuingOutputs ctx of
        [o] -> o
        _ ->
            traceError
                $ "checkAddPayment: Found multiple output UTxO where "
                `appendString` "only one was expected."

    outputState :: EscrowState
    outputState = case getTxDatum output ctx of
        Just (EscrowDatum escrowState) -> escrowState
        _ ->
            traceError "checkAddPayment: Output UTxO datum could not be found."

    inputValue, outputValue :: Value
    inputValue  = txOutValue input
    outputValue = txOutValue output

    checkDatum :: Bool
    checkDatum = case addPayment st pkh m of
        (Just newEscrowState) -> outputState == newEscrowState
        _                     -> False

    checkValue :: Bool
    checkValue = outputValue == inputValue <> lovelaceValueOf m

    checkMinting :: Bool
    checkMinting = txInfoMint (scriptContextTxInfo ctx) == mempty

-- | Validation for 'collect' operation when the Escrow UTxO is spent.
--   In the following case, from the standpoint of the Escrow script UTxO, we
--   must check that:
--      * the NFT is in the output UTxO,
--      * the Datum in the output UTxO is the same as applying the business
--        logic to the input UTxO,
--      * the Value in the output UTxO is the same as the inputValue combined
--        with the collected amount,
--      * the signer's pkh is in the EscrowState,
--      * we are not minting any tokens.
{-# INLINABLE validateCollect #-}
validateCollect :: Parameter -> EscrowState -> ScriptContext -> Bool
validateCollect p st ctx =
       debug "checkCollect: NFT missing in the output UTxO."
           (outputHasNFT (stateNFT p) ctx)
    && debug "checkCollect: Output UTxO datum is wrong."     checkDatum
    && debug "checkCollect: Amount collected is wrong."      checkValue
    && debug "checkCollect: The signer's pkh was not found." pkhHasDeposit
    && debug "checkCollect: Tokens are minted."              checkMinting
  where
    input, output :: TxOut
    input = case findOwnInput ctx of
        Just txInInfo -> txInInfoResolved txInInfo
        _ ->
            traceError "checkCollect: Input UTxO not found." (findOwnInput ctx)
    output = case getContinuingOutputs ctx of
        [o] -> o
        _ ->
            traceError
                $              "checkCollect: Found multiple output UTxO where "
                `appendString` "only one was expected."

    outputState :: EscrowState
    outputState = case getTxDatum output ctx of
        Just (EscrowDatum escrowState) -> escrowState
        _ -> traceError "checkCollect: Output UTxO datum could not be found."

    inputValue, outputValue :: Value
    inputValue  = txOutValue input
    outputValue = txOutValue output

    signer :: PaymentPubKeyHash
    signer = case txInfoSignatories (scriptContextTxInfo ctx) of
        [pkh] -> PaymentPubKeyHash { unPaymentPubKeyHash = pkh }
        _     -> traceError "checkCollect: Signer could not be found."

    pkhHasDeposit :: Bool
    pkhHasDeposit = signer `elem` receivers st

    checkDatum :: Bool
    checkDatum = case collect st signer of
        Just (newEscrowState, _) -> newEscrowState == outputState
        _                        -> True

    checkValue :: Bool
    checkValue = case collect st signer of
        Just (_, amountCollected) ->
            outputValue == inputValue PNum.- lovelaceValueOf amountCollected
        _ -> True

    checkMinting :: Bool
    checkMinting = txInfoMint (scriptContextTxInfo ctx) == mempty
