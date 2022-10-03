{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Utils.OffChain
Description : Common off-chain functions.
Copyright   : (c) 2021 IDYIA LLC dba imagine.ai
Maintainer  : sos@imagine.ai
Stability   : develop
-}

module Utils.OffChain where

import           Data.Text         (Text)
import qualified Data.Text         as T
import qualified Data.Map          as Map
import           Data.Monoid       (Last (..))

import           Control.Lens
import           Control.Monad
import           Prelude           hiding ((*))

import qualified PlutusTx
import           PlutusTx.Prelude (modulo)
import           Ledger            hiding (singleton)
import           Ledger.Value
import qualified Ledger.Ada        as Ada
import           Ledger.Constraints as Constraints
import           Ledger.Typed.Scripts
import           Plutus.Contract   hiding (tell)
import qualified Plutus.Contract   as Contract (tell)
import           PlutusTx.Numeric  as Num
import           Plutus.ChainIndex (TxValidity, RollbackState(Committed))

-- import Concurrent.Types

{- | Off-chain function for getting a list of utxos for the given address that
     contains the given NFT. The ChainIndexTxOut, if possible, has
     the datum field loaded with the correct datum result of calling
    `datumFromHash`.
-}
lookupScriptUtxos
    :: forall w s
    .  Address
    -> AssetClass
    -> Contract w s Text [(TxOutRef, ChainIndexTxOut)]
lookupScriptUtxos addr nft =
    utxosAt addr >>=
    (mapM (\ (oref, o) -> ciTxOutDatum loadDatum o <&> (oref,)) . Map.toList)
    . Map.filter (checkTxHasNFT nft . (^. ciTxOutValue))
  where
    loadDatum
        :: Either DatumHash Datum
        -> Contract w s T.Text (Either DatumHash Datum)
    loadDatum lhd@(Left dh) = maybe lhd Right <$> datumFromHash dh
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
    -> Contract w s Text (TxOutRef, ChainIndexTxOut)
lookupScriptUtxo addr nft = lookupScriptUtxos addr nft >>=
    \case
        [utxo] -> return utxo
        _      -> throwError $ T.unwords
                     [ "Can't find the unique utxo of address"
                     , T.pack $ show addr
                     , "with the nft"
                     , T.pack $ show nft
                     ]

{- | Get the datum from a chainIndexTxOut, only if isn't a datum hash. This
     pure function doesn't call `datumFromHash`.
-}
getChainIndexTxOutDatum
    :: PlutusTx.FromData d
    => ChainIndexTxOut
    -> Maybe d
getChainIndexTxOutDatum ciTxOut =
    case matching _ScriptChainIndexTxOut ciTxOut of
        Right (_,_,Right d,_) -> PlutusTx.fromBuiltinData $ getDatum d
        _ -> Nothing

{-# INLINABLE minAdaTimes #-}
minAdaTimes :: Integer -> Value
minAdaTimes n =
    let Ada.Lovelace {Ada.getLovelace = m} = Ledger.minAdaTxOut
    in Ada.lovelaceValueOf (n * m)

{-# INLINABLE negativeMinAdaTimes #-}
negativeMinAdaTimes :: Integer -> Value
negativeMinAdaTimes = minAdaTimes . Num.negate

-- | The type 'LastTxId' sums up either a transaction id or another type.
type LastTxId w = Last (Either (TxId, TxValidity) w)

-- | Given a value of type 'w' we wrap up that value with the 'Right' constructor.
tell :: forall w s. w -> Contract (LastTxId w) s Text ()
tell = Contract.tell . pure . Right

-- | Given a 'TxId' we wrap up that value with the 'Left' constructor.
tellTxId
    :: forall w s
    .  TxId
    -> Contract (LastTxId w) s Text ()
tellTxId txid = awaitTxCommited txid >>=
                \txv -> Contract.tell (pure $ Left (txid, txv))

{- | REMOVE ME on the near future. This function awaits until the status of the
     transaction is commmitted and it only make sense for a backend integration
     using the simulated PAB or the trace emulator.
-}
awaitTxCommited
    :: forall w s
    .  TxId
    -> Contract w s Text TxValidity
awaitTxCommited i = go
  where
    go :: Contract w s Text TxValidity
    go = awaitTxStatusChange i >>=
         \case Committed txv _ -> return txv
               _             -> go

selectRandom :: AsContractError e => [a] -> Contract w s e (Maybe a)
selectRandom [] = return Nothing
selectRandom xs = do
    t <- currentTime
    return . Just $ xs !! fromIntegral (getPOSIXTime t `modulo` toInteger (length xs))

submitTxConstraintsWithLogging
    :: ( PlutusTx.FromData (DatumType a)
       , PlutusTx.ToData (DatumType a)
       , PlutusTx.ToData (RedeemerType a)
       )
    => ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> Contract w s Text ()
submitTxConstraintsWithLogging lkp tx = case mkTx lkp tx of
    Left e -> logInfo @String $ show e
    Right utx -> balanceTx utx >>= \cardanoTx -> do
        logInfo @String $ "Unbalanced tx -> " ++ show utx
        logInfo @String $ "Balanced tx -> " ++ show cardanoTx
        void $ submitBalancedTx cardanoTx

isCustodial :: Bool
isCustodial = False

handleTxConstraints
    :: ( PlutusTx.ToData (RedeemerType a)
       , PlutusTx.FromData (DatumType a)
       , PlutusTx.ToData (DatumType a)
       )
    => ScriptLookups a
    -> TxConstraints (RedeemerType a) (DatumType a)
    -> Contract w s T.Text ()
handleTxConstraints lkp tx =
    if isCustodial
        then void $ submitTxConstraintsWith lkp tx
        else mkTxConstraints lkp tx >>= yieldUnbalancedTx . adjustUnbalancedTx

-- mustPayToWalletAddress
--     :: forall i o
--     .  WalletAddress
--     -> Value
--     -> TxConstraints i o
-- mustPayToWalletAddress WalletAddress{..} val = case waStaking of
--     Just staking ->
--         let ppkh = PaymentPubKeyHash waPayment
--             spkh = StakePubKeyHash staking
--         in Constraints.mustPayToPubKeyAddress ppkh spkh val
--     Nothing -> Constraints.mustPayToPubKey (PaymentPubKeyHash waPayment) val