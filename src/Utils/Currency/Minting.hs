{-# LANGUAGE DataKinds                        #-}
{-# LANGUAGE DerivingStrategies               #-}
{-# LANGUAGE ImportQualifiedPost              #-}
{-# LANGUAGE MultiParamTypeClasses            #-}
{-# LANGUAGE LambdaCase                       #-}
{-# LANGUAGE OverloadedStrings                #-}
{-# LANGUAGE ScopedTypeVariables              #-}
{-# LANGUAGE TemplateHaskell                  #-}
{-# LANGUAGE TupleSections                  #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}

module Utils.Currency.Minting (
      Currency(..)
    , MintingPolicyAction(..)
    , C.CurrencyError(..)
    , C.AsCurrencyError(..)
    , curSymbol
    , mintCurrencyWithPpkh
    , mintNFTWithPpkh
    , mintNFTWithUTxO
    , mintCurrencyWithUTxO
    , burnNFT
    , burnCurrency
    ) where

import Control.Lens
import Data.Maybe                hiding (isJust)
import Data.Text                 qualified as T
import Prelude                   ((<>))
import Prelude                   qualified as H

import PlutusTx                  qualified
import PlutusTx.Prelude          hiding (Monoid (..), Semigroup (..))
import Plutus.Contract           as Contract
import Plutus.Contracts.Currency qualified as C
import Plutus.V1.Ledger.Value    as Value
import Data.Map.Internal         qualified as Map
import Ledger                    hiding (Minting)
import Ledger.Constraints        qualified as Constraints
import Ledger.Constraints        (ScriptLookups, TxConstraints)
import Ledger.Contexts           qualified as V
import Ledger.Typed.Scripts      qualified as Scripts

import Utils.OffChain            (minAdaTimes, selectRandom)

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

mintingRed :: Redeemer
mintingRed = Redeemer $ PlutusTx.toBuiltinData Minting

burningRed :: Redeemer
burningRed = Redeemer $ PlutusTx.toBuiltinData Burning

{-| To mint an NFT without submitting a dedicated transaction. Through this
    function we provide the needed lookups and constraints for doing it.
-}
mintNFTWithPpkh :: forall c a w s i o .
       Currency c
    => TokenName
    -> PaymentPubKeyHash
    -> Contract w s T.Text (TxOutRef, c, ScriptLookups a, TxConstraints i o)
mintNFTWithPpkh tkn = mintCurrencyWithPpkh [(tkn, 1)]

{-| This functions returns the currency and the lookups and constraints needed
    to mint it in a transaction: does not creates a dedicated transaction for it
-}
mintCurrencyWithPpkh :: forall c a w s i o .
       Currency c
    => [(TokenName, Integer)]
    -> PaymentPubKeyHash
    -> Contract w s T.Text (TxOutRef, c, ScriptLookups a, TxConstraints i o)
mintCurrencyWithPpkh tkn = mapError (T.pack . H.show) . mintTxWithPpkh tkn

{-| `mintTx` does not submit a transaction giving the tokens to the wallet, that
    is what `mintContract` do.
    Instead, `mintTx` returns the needed lookups and constraints for a given
    operation to include the minting-tokens-related information to build a
    transaction that will _include_ the minting.
-}
mintTxWithPpkh :: forall c w s a i o
               .  Currency c
               => [(TokenName, Integer)]
               -> PaymentPubKeyHash
               -> Contract w s C.CurrencyError ( TxOutRef
                                               , c
                                               , ScriptLookups a
                                               , TxConstraints i o
                                               )
mintTxWithPpkh amounts ppkh = mapError (review C._CurrencyError) .
    selectUTxO $ \(ref, ciTxOut) -> do
        let theCurrency = mkCurrency ref
            mintedVal = mintedValue theCurrency amounts
            lookups = Constraints.mintingPolicy (curPolicy theCurrency)
                   <> Constraints.unspentOutputs (Map.singleton ref ciTxOut)
            tx = Constraints.mustSpendPubKeyOutput ref
              <> Constraints.mustMintValueWithRedeemer mintingRed mintedVal
        pure (ref, theCurrency, lookups, tx)
  where
    selectUTxO :: ( (TxOutRef, ChainIndexTxOut) ->
                    Contract w
                             s
                             C.CurrencyError
                             (TxOutRef, c, ScriptLookups a, TxConstraints i o)
                  )
               -> Contract w s C.CurrencyError ( TxOutRef
                                               , c
                                               , ScriptLookups a
                                               , TxConstraints i o
                                               )
    selectUTxO continue = ownPpkhUTxOs >>= selectRandom
        >>= \case Nothing -> noUTxOsInPpkhError
                  Just (ref, ciTxOut) -> continue (ref, ciTxOut)

    -- | Returns the ppkh UTxOs that has _more_ than 2*minAda.
    ownPpkhUTxOs :: Contract w s C.CurrencyError [(TxOutRef, ChainIndexTxOut)]
    ownPpkhUTxOs = utxosAt (pubKeyHashAddress ppkh Nothing)
        <&> Map.toList . Map.filter ((`gt` minAdaTimes 2) . (^. ciTxOutValue))

    -- | Not UTxOs with more than 2*minAda in the ppkh.
    noUTxOsInPpkhError :: Contract w s C.CurrencyError b
    noUTxOsInPpkhError = throwError . C.CurContractError . OtherContractError
                       . T.append "No utxos in pkh = "
                       . T.pack . H.show $ ppkh

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
               -> Contract w s C.CurrencyError ( c
                                               , ScriptLookups a
                                               , TxConstraints i o
                                               )
mintTxWithUTxO amounts ref = mapError (review C._CurrencyError) .
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
                                     C.CurrencyError
                                     (c, ScriptLookups a, TxConstraints i o)
                     )
                  -> Contract w s C.CurrencyError ( c
                                                  , ScriptLookups a
                                                  , TxConstraints i o
                                                  )
    lookupCiTxOut continue = unspentTxOutFromRef ref >>=
        \case Nothing -> throwError . C.CurContractError . OtherContractError
                         $ T.append (T.pack . H.show $ ref)
                                    " does not have its corresponding ciTxOut."
              Just ciTxOut -> continue ciTxOut

    mustSpendPubKeyOutputIfTxOutIsFromWallet :: TxOutRef
                                             -> ChainIndexTxOut
                                             -> TxConstraints i o
    mustSpendPubKeyOutputIfTxOutIsFromWallet ref_ = \case
        PublicKeyChainIndexTxOut{} -> Constraints.mustSpendPubKeyOutput ref_
        ScriptChainIndexTxOut{}    -> H.mempty

{-| Returns lookups and constraints to burn a NFT minted with this module.
    It uses the currency symbol of the asset class given as argument for finding
    the minting policy associated to the NFT.
-}
burnNFT :: forall w s a i o .
           AssetClass
        -> Contract w s T.Text ( ScriptLookups a
                               , TxConstraints i o
                               )
burnNFT ac = burnCurrency (ac, 1)

burnCurrency :: forall w s a i o .
                (AssetClass, Integer)
             -> Contract w s T.Text ( ScriptLookups a
                                    , TxConstraints i o
                                    )
burnCurrency (currAC, amount) = getMintingPolicyFromAC currAC $ \mp ->
    let negCurrencyVal = assetClassValue currAC (-amount)
        lkp = Constraints.mintingPolicy mp
        tx = Constraints.mustMintValueWithRedeemer burningRed negCurrencyVal
    in pure (lkp, tx)
  where
    getMintingPolicyFromAC :: AssetClass
                           -> ( MintingPolicy ->
                                Contract w s T.Text ( ScriptLookups a
                                                    , TxConstraints i o
                                                    )
                              )
                           -> Contract w s T.Text ( ScriptLookups a
                                                  , TxConstraints i o
                                                  )
    getMintingPolicyFromAC ac continue =
        mintingPolicyFromHash (mintingPolicyHashFromAC ac) >>=
            \case Just mp -> continue mp
                  Nothing -> throwError "Minting policy not found!"

    mintingPolicyHashFromAC :: AssetClass -> MintingPolicyHash
    mintingPolicyHashFromAC = MintingPolicyHash . unCurrencySymbol
                            . fst . unAssetClass


PlutusTx.unstableMakeIsData ''MintingPolicyAction
