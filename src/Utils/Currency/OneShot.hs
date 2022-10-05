{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

{-|
Module      : Utils.Currency.OneShot
Description : Minting and burning currency abstraction.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

Functions related to OneShot currency (analogous to OneShotCurrency of Plutus)
-}
module Utils.Currency.OneShot
    ( OneShot
    ) where

import Ledger                 hiding (Minting)
import GHC.Generics           (Generic)
import Prelude                qualified as H
import Data.Aeson             (ToJSON, FromJSON)
import Ledger.Typed.Scripts   qualified as Scripts
import PlutusTx               qualified
import PlutusTx.Prelude

import Utils.Currency.Minting (Currency(..), MintingPolicyAction(..))

newtype OneShot = OneShot { curTxOutRef :: TxOutRef }
  deriving stock (Generic, H.Show, H.Eq)
  deriving anyclass (ToJSON, FromJSON)

instance Currency OneShot where
    {-# INLINEABLE mkCurrency #-}
    mkCurrency :: TxOutRef -> OneShot
    mkCurrency ref = OneShot { curTxOutRef = ref }

    {-# INLINEABLE getRef #-}
    getRef :: OneShot -> TxOutRef
    getRef = curTxOutRef

    {-# INLINEABLE curPolicy #-}
    curPolicy :: OneShot -> Scripts.MintingPolicy
    curPolicy cur = mkMintingPolicyScript $
        $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . checkPol ||])
            `PlutusTx.applyCode` PlutusTx.liftCode cur
      where
        checkPol = checkPolicy @OneShot

    {-# INLINEABLE checkPolicy #-}
    checkPolicy :: OneShot -> MintingPolicyAction -> ScriptContext -> Bool
    checkPolicy c ac ctx = case ac of
        Minting -> let txInfo = scriptContextTxInfo ctx
                       TxOutRef h i = curTxOutRef c
                   in traceIfFalse
                        "Does not spend the designated transaction output"
                        (spendsOutput txInfo h i)
        Burning -> False

-- | Plutus boilerplate.
PlutusTx.makeLift ''OneShot
