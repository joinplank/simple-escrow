{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

{-|
Module      : Escrow.Tokens
Description : Definition of token names and utilities.
Copyright   : (c) 2022 IDYIA LLC dba imagine.ai
Maintainer  : sos@imagine.ai
Stability   : develop
-}

module Escrow.Tokens where

import Data.Text

import Ledger                                hiding (singleton)
import Ledger.Value              as Value
import Plutus.Contract           as Contract

import Utils.Currency as Currency
import Plutus.Contracts.Currency


-- | Token name for the contract NFT.
escrowTokenName :: TokenName
escrowTokenName = "escrowToken"

-- | Gets only the value of the given asset class.
getValueOf :: AssetClass -> Value -> Value
getValueOf asc v = assetClassValue asc $ assetClassValueOf v asc

-- | Utility for forging an NFT.
forgeNFT
    :: PaymentPubKeyHash
    -> TokenName
    -> Contract w s Text OneShotCurrency
forgeNFT pkh tkn = mapError (pack . show @Currency.CurrencyError) $
                   mintContract pkh [(tkn, 1)]
