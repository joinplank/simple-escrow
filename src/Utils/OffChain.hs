{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE LambdaCase            #-}

{-|
Module      : Utils.OffChain
Description : Common off-chain functions.
Copyright   : (c) 2021 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.OffChain where

import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Map          as Map

import           Control.Lens
import           Control.Monad
import           Prelude           hiding ((*))

import           Ledger            hiding (singleton)
import qualified Ledger.Ada         as Ada
import           Ledger.Constraints as Constraints
import           Ledger.Typed.Scripts
import           Ledger.Value
import qualified PlutusTx
import           Plutus.Contract   as Contract
import           PlutusTx.Numeric  as Num

{- | Off-chain function for getting a list of utxos for the given address that
     contains the given NFT. The ChainIndexTxOut, if possible, has
     the datum field loaded with the correct datum result of calling
    `datumFromHash`.
-}
lookupScriptUtxos
    :: forall w s
    .  Address
    -> AssetClass
    -> Contract w s Text [(TxOutRef, DecoratedTxOut)]
lookupScriptUtxos addr nft =
    utxosAt addr >>=
    (mapM (\ (oref, o) -> decoratedTxOutDatum loadDatum o <&> (oref,))
    .Map.toList
    . Map.filter (checkTxHasNFT nft . (^. decoratedTxOutValue)))
  where
    loadDatum
        :: (DatumHash, DatumFromQuery)
        -> Contract w s T.Text (DatumHash, DatumFromQuery)
    loadDatum d@(dh, DatumUnknown) =  maybe d ((dh,) . DatumInBody)
                                      <$>
                                      datumFromHash dh
    loadDatum d = return d

    checkTxHasNFT :: AssetClass -> Value -> Bool
    checkTxHasNFT asc v = assetClassValueOf v asc == 1

{- | Off-chain function for getting the unique utxo for the given address that
     contains the given NFT. The ChainIndexTxOut, if possible, has
     the datum field loaded with the correct datum result of calling
    `datumFromHash`.
-}
lookupScriptUtxo
    :: forall w s
    .  Address
    -> AssetClass
    -> Contract w s Text (TxOutRef, DecoratedTxOut)
lookupScriptUtxo addr nft = lookupScriptUtxos addr nft >>=
    \case
        [utxo] -> return utxo
        _      -> throwError $ T.unwords
                     [ "Can't find the unique utxo of address"
                     , T.pack $ show addr
                     , "with the nft"
                     , T.pack $ show nft
                     ]

{- | Get the datum from a DecoratedTxOut, only if isn't a datum hash. This
     pure function doesn't call `datumFromHash`.
-}
getDecoratedTxOutDatum
    :: PlutusTx.FromData d
    => DecoratedTxOut
    -> Maybe d
getDecoratedTxOutDatum dTxOut =
    case matching _ScriptDecoratedTxOut dTxOut of
        Right (_,_,_,(_,DatumInline d),_,_) -> PlutusTx.fromBuiltinData $ getDatum d
        Right (_,_,_,(_,DatumInBody d),_,_) -> PlutusTx.fromBuiltinData $ getDatum d
        _ -> Nothing

-- | Gets only the value of the given asset class.
getValueOf :: AssetClass -> Value -> Value
getValueOf asc v = assetClassValue asc $ assetClassValueOf v asc

{-# INLINABLE minAdaTimes #-}
minAdaTimes :: Integer -> Value
minAdaTimes n = let Ada.Lovelace {Ada.getLovelace = m} = Ledger.minAdaTxOut
                in Ada.lovelaceValueOf (n * m)

{-# INLINABLE negativeMinAdaTimes #-}
negativeMinAdaTimes :: Integer -> Value
negativeMinAdaTimes = minAdaTimes . Num.negate

{- | Given some lookups and tx-constraints, builds the unbalance transaction,
     adjust the output UTxOs with min-ada (if needed), and yield it.
-}
handleTxConstraints :: ( PlutusTx.ToData (RedeemerType a)
                       , PlutusTx.FromData (DatumType a)
                       , PlutusTx.ToData (DatumType a)
                       )
                    => ScriptLookups a
                    -> TxConstraints (RedeemerType a)
                                     (DatumType a)
                    -> Contract w s T.Text ()
handleTxConstraints lkp tx = mkTxConstraints lkp tx >>=
                             Contract.adjustUnbalancedTx >>=
                             yieldUnbalancedTx
