{-|
Module      : Utils.Currency
Description : Minting and burning currency abstraction.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Utils.Currency
    (   -- | Currency functions
      Currency(..)
    , curSymbol
    , MintingPolicyAction(..)
        -- | Currency types
    , OneShot
        -- | Currency errors
    , CurrencyError(..)
    , AsCurrencyError(..)
        -- | Minting functions
    , mintNFTWithUTxO
    , mintCurrencyWithUTxO
    ) where

import Utils.Currency.Minting ( Currency(..)
                              , MintingPolicyAction(..)
                              , curSymbol
                              , mintNFTWithUTxO
                              , mintCurrencyWithUTxO
                              , AsCurrencyError(..)
                              , CurrencyError(..)
                              )

import Utils.Currency.OneShot (OneShot)
