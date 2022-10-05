{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}

{-|
Module      : Utils.OnChain
Description : Useful functions about script contexts.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

Common functions for manipulating script context.
-}

module Utils.OnChain where


import           Ledger            hiding (singleton)
import qualified Ledger.Ada        as Ada
import           Ledger.Value      as Value
import qualified PlutusTx
import qualified PlutusTx.AssocMap as M
import           PlutusTx.Maybe    as Maybe
import           PlutusTx.Prelude  as PP hiding (mapMaybe)

import           Escrow.Types

-- | Gets all utxo inputs corresponding with address addr.
{-# INLINABLE getTxInputs #-}
getTxInputs :: Address -> ScriptContext -> [TxInInfo]
getTxInputs addr ctx = [ i | i <- txInfoInputs info
                           , txOutAddress (txInInfoResolved i) == addr
                       ]
  where
    info = scriptContextTxInfo ctx

-- | Gets all utxo outputs corresponding with address addr.
{-# INLINABLE getTxOutputs #-}
getTxOutputs :: Address -> ScriptContext -> [TxOut]
getTxOutputs addr ctx = [ o | o <- txInfoOutputs info
                            , txOutAddress o == addr
                        ]
  where
    info = scriptContextTxInfo ctx

-- | Gets all utxo inputs with script addresses.
{-# INLINABLE getAllScriptInputs #-}
getAllScriptInputs :: ScriptContext -> [TxInInfo]
getAllScriptInputs =
  filterInputs (isJust . toValidatorHash . txOutAddress . txInInfoResolved)

-- | Gets all utxo inputs with public key addresses.
{-# INLINABLE getAllWalletInputs #-}
getAllWalletInputs :: ScriptContext -> [TxInInfo]
getAllWalletInputs =
  filterInputs (isJust . toValidatorHash . txOutAddress . txInInfoResolved)

-- | Filter utxo inputs.
{-# INLINABLE filterInputs #-}
filterInputs :: (TxInInfo -> Bool) -> ScriptContext -> [TxInInfo]
filterInputs f = filter f . (txInfoInputs . scriptContextTxInfo)

-- | Obtains the unique script utxo which has the corresponding NFT.
{-# INLINABLE getTxInFromNFT #-}
getTxInFromNFT :: ScriptContext -> AssetClass -> Maybe TxInInfo
getTxInFromNFT ctx nft =
    case filterInputs (checkTxHasNFT nft . txInInfoResolved) ctx of
        [o] -> Just o
        _   -> Nothing

-- | Filter utxo outputs.
{-# INLINABLE filterOutputs #-}
filterOutputs :: (TxOut -> Bool) -> ScriptContext -> [TxOut]
filterOutputs f = filter f . (txInfoOutputs . scriptContextTxInfo)

-- | Filter all the utxo outputs that pay to the same script address
--   that we are currently spending from.
{-# INLINABLE filterContinuingOutputs #-}
filterContinuingOutputs
    :: (TxOut -> Bool)
    -> ScriptContext
    -> [TxOut]
filterContinuingOutputs f = filter f . getContinuingOutputs

-- | Obtains the unique script utxo which has the corresponding NFT.
{-# INLINABLE getTxOutFromNFT #-}
getTxOutFromNFT :: ScriptContext -> AssetClass -> Maybe TxOut
getTxOutFromNFT ctx nft =
    case filterContinuingOutputs (checkTxHasNFT nft) ctx of
        [o] -> Just o
        _   -> Nothing

-- | Checks that a transaction has attached the specified NFT.
{-# INLINABLE checkTxHasNFT #-}
checkTxHasNFT :: AssetClass -> TxOut -> Bool
checkTxHasNFT asc o = assetClassValueOf (txOutValue o) asc == 1

-- | Checks that the own script input contains the specified NFT.
{-# INLINABLE inputHasNFT #-}
inputHasNFT :: AssetClass -> ScriptContext -> Bool
inputHasNFT asc =
  maybe (traceError "script input missing")
        (checkTxHasNFT asc . txInInfoResolved) . findOwnInput

-- | Checks that the own script output contains the specified NFT.
{-# INLINABLE outputHasNFT #-}
outputHasNFT :: AssetClass -> ScriptContext -> Bool
outputHasNFT asc = maybe (traceError "script unique own output missing")
                         (checkTxHasNFT asc) . uniqueScriptOutput

-- | Gets the datum attached to a utxo.
{-# INLINABLE getTxDatum #-}
getTxDatum
    :: forall d
    . PlutusTx.FromData d
    => TxOut
    -> ScriptContext
    -> Maybe d
getTxDatum o ctx = txOutDatum o >>= (`findDatum` scriptContextTxInfo ctx)
                   >>= PlutusTx.fromBuiltinData . getDatum

-- | Obtains the unique script utxo if there exists.
{-# INLINABLE uniqueScriptOutput #-}
uniqueScriptOutput :: ScriptContext -> Maybe TxOut
uniqueScriptOutput ctx = case getContinuingOutputs ctx of
                           [o] -> Just o
                           _   -> Nothing

-- | Check the value of the unique script input and output doesn't changed.
{-# INLINABLE unchangedValueFromNFT #-}
unchangedValueFromNFT :: ScriptContext -> AssetClass -> Bool
unchangedValueFromNFT ctx asc =
    traceIfFalse "unchangedValueFromNFT: Changed value" $ inVal == outVal
  where
    inVal :: Value
    inVal = maybe
            (traceError "unchangedValueFromNFT inVal: missing utxo input")
            (txOutValue . txInInfoResolved)
            (findOwnInput ctx)

    outVal :: Value
    outVal = maybe
             (traceError "unchangedValueFromNFT outVal: missing utxo output")
             txOutValue
             (getTxOutFromNFT ctx asc)

-- | Check the value of the unique script input and output doesn't changed.
{-# INLINABLE unchangedValue #-}
unchangedValue :: ScriptContext -> Bool
unchangedValue ctx =
    traceIfFalse "unchangedValue: Changed value" $ inVal == outVal
  where
    inVal :: Value
    inVal = maybe (traceError "unchangedValue inVal: missing utxo input")
                  (txOutValue . txInInfoResolved)
                  (findOwnInput ctx)

    outVal :: Value
    outVal = maybe (traceError "unchangedValue outVal: missing utxo output")
                   txOutValue
                   (uniqueScriptOutput ctx)

{-# INLINABLE equalValues #-}
equalValues :: Value -> Value -> Bool
equalValues v1 v2 = getValue v1 == getValue v2

{-# INLINABLE singletonMinAda #-}
singletonMinAda :: AssetClass -> Integer -> Value
singletonMinAda ac i = minAda <> assetClassValue ac i

{-# INLINABLE addSingleton #-}
addSingleton :: Value -> AssetClass -> Integer -> Value
addSingleton (Value v) (AssetClass (c, tn)) i = Value
    (M.fromList $ (c, M.singleton tn i) : M.toList v)

{-# INLINABLE addMinAda #-}
addMinAda :: Value -> Value
addMinAda (Value v) = Value
  (M.fromList $ (Ada.adaSymbol, M.singleton Ada.adaToken 2000000) : M.toList v)

{-# INLINABLE isSingleton #-}
isSingleton :: [a] -> Bool
isSingleton [_] = True
isSingleton _ = False

{-# INLINABLE drop #-}
drop :: Integer -> [a] -> [a]
drop _ [] = []
drop n l@(_:xs) | n == 0    = l
                | otherwise = drop (n-1) xs

{-# INLINABLE debug #-}
debug :: BuiltinString -> Bool -> Bool
debug = traceIfFalse -- const id

{-# INLINABLE fromJust #-}
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust _ = traceError "fromJust"
