{-# LANGUAGE DataKinds                        #-}
{-# LANGUAGE DeriveGeneric                    #-}
{-# LANGUAGE DeriveAnyClass                   #-}
{-# LANGUAGE DerivingStrategies               #-}
{-# LANGUAGE ImportQualifiedPost              #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE LambdaCase                       #-}
{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE TemplateHaskell                  #-}
{-# LANGUAGE TupleSections                  #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

{-|
Module      : Utils.Currency.Minting
Description : Minting and burning currency abstraction.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.Currency.Minting (
      Currency(..)
    , MintingPolicyAction(..)
    , CurrencyError(..)
    , AsCurrencyError(..)
    , curSymbol
    , mintNFTWithUTxO
    , mintCurrencyWithUTxO
    ) where

import Control.Lens
import Data.Maybe                       hiding (isJust)
import Data.Text                        qualified as T
import Data.Aeson                       (FromJSON, ToJSON)
import GHC.Generics                     (Generic)
import Prelude                          ((<>))
import Prelude                          qualified as H

import PlutusTx                         qualified
import PlutusTx.Prelude                 hiding (Monoid (..), Semigroup (..))
import Plutus.Contract                  as Contract
import Plutus.Script.Utils.V1.Scripts   (scriptCurrencySymbol)
import Plutus.V1.Ledger.Value           as Value
import Plutus.V1.Ledger.Contexts        qualified as V
import Data.Map.Internal                qualified as Map
import Ledger                           hiding (Minting)
import Ledger.Constraints               qualified as Constraints
import Ledger.Constraints               (ScriptLookups, TxConstraints)
import Ledger.Scripts                   qualified as Scripts

newtype CurrencyError =
    CurContractError ContractError
    deriving stock (H.Eq, H.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

makeClassyPrisms ''CurrencyError

instance AsContractError CurrencyError where
    _ContractError = _CurContractError

{-| The idea behind this typeclass is that when we want to define a new currency
    to be minted in our contract, we only have to specify the definitions for
    this functions so we can reuse `mintNFT` and `mintCurrency`.
-}
class Currency c where
    {-| Smart constructor for the currency. This restricts the currency type to
        have a TxOutRef as its unique field. It needs to be extended in the
        future if a currency needs another argument to its constructor apart
        form a TxOutRef (and also if it does not needs it at all!).
    -}
    mkCurrency :: TxOutRef -> c

    getRef :: c -> TxOutRef

    {-| The boilerplate definition that uses template haskell to compile to
        Plutus core. Because of that usage of template haskell, I cannot
        implement just one time this function for every type that implements
        the Currency typeclass: it needs to know the exact type at compilation
        time so the boilerplate needs to be made for every instance.
    -}
    curPolicy :: c -> Scripts.MintingPolicy

    -- | The predicate that restricts the currency minting.
    checkPolicy :: c -> MintingPolicyAction -> V.ScriptContext -> Bool

{-# INLINEABLE curSymbol #-}
curSymbol :: Currency c => c -> CurrencySymbol
curSymbol = scriptCurrencySymbol . curPolicy

-- | Given the currency and its amount, returns the value to be minted.
mintedValue :: Currency c => c -> [(TokenName, Integer)] -> Value
mintedValue = foldMap . uncurry . Value.singleton . curSymbol

{-| This type represents the redeemer to be specified in the
    `mustMintValueWithRedeemer` constraint when minting or burning currencies.
-}
data MintingPolicyAction = Minting | Burning

mintingRed :: Scripts.Redeemer
mintingRed = Scripts.Redeemer $ PlutusTx.toBuiltinData Minting

{-| Returns lookups and constraints to mint an NFT using a specific TxOutRef.
-}
mintNFTWithUTxO :: forall c a w s i o
                .  Currency c
                => TokenName
                -> TxOutRef
                -> Contract w s T.Text ( c
                                       , ScriptLookups a
                                       , TxConstraints i o
                                       )
mintNFTWithUTxO tkn = mintCurrencyWithUTxO [(tkn, 1)]

{-| Returns lookups and constraints to mint a currency using a specific TxOutRef.
-}
mintCurrencyWithUTxO :: forall c a w s i o
                     .  Currency c
                     => [(TokenName, Integer)]
                     -> TxOutRef
                     -> Contract w s T.Text ( c
                                            , ScriptLookups a
                                            , TxConstraints i o
                                            )
mintCurrencyWithUTxO tkn =
    mapError (T.pack . H.show) . mintTxWithUTxO tkn

mintTxWithUTxO :: forall c w s a i o
               .  Currency c
               => [(TokenName, Integer)]
               -> TxOutRef
               -> Contract w s CurrencyError ( c
                                               , ScriptLookups a
                                               , TxConstraints i o
                                               )
mintTxWithUTxO amounts ref = mapError (review _CurrencyError) .
    lookupCiTxOut $ \ciTxOut -> do
        let theCurrency = mkCurrency ref
            mintedVal = mintedValue theCurrency amounts
            lookups = Constraints.mintingPolicy (curPolicy theCurrency)
                   <> Constraints.unspentOutputs (Map.singleton ref ciTxOut)
            tx = Constraints.mustMintValueWithRedeemer mintingRed mintedVal
              <> mustSpendPubKeyOutputIfTxOutIsFromWallet ref ciTxOut
        pure (theCurrency, lookups, tx)
  where
    lookupCiTxOut :: ( ChainIndexTxOut ->
                            Contract w
                                     s
                                     CurrencyError
                                     (c, ScriptLookups a, TxConstraints i o)
                     )
                  -> Contract w s CurrencyError ( c
                                                  , ScriptLookups a
                                                  , TxConstraints i o
                                                  )
    lookupCiTxOut continue = unspentTxOutFromRef ref >>=
        \case Nothing -> throwError . CurContractError . OtherContractError
                         $ T.append (T.pack . H.show $ ref)
                                    " does not have its corresponding ciTxOut."
              Just ciTxOut -> continue ciTxOut

    mustSpendPubKeyOutputIfTxOutIsFromWallet :: TxOutRef
                                             -> ChainIndexTxOut
                                             -> TxConstraints i o
    mustSpendPubKeyOutputIfTxOutIsFromWallet ref_ = \case
        PublicKeyChainIndexTxOut{} -> Constraints.mustSpendPubKeyOutput ref_
        ScriptChainIndexTxOut{}    -> H.mempty

PlutusTx.makeIsDataIndexed ''MintingPolicyAction [ ('Minting, 0)
                                                 , ('Burning, 1)
                                                 ]
