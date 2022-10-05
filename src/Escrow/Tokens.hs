{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Escrow.Tokens
Description : Definition of token names and utilities.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Escrow.Tokens where

import Ledger       hiding (singleton)
import Ledger.Value as Value

-- | Token name for the contract NFT.
escrowTokenName :: TokenName
escrowTokenName = "escrowToken"

-- | Gets only the value of the given asset class.
getValueOf :: AssetClass -> Value -> Value
getValueOf asc v = assetClassValue asc $ assetClassValueOf v asc
