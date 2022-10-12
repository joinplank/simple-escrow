{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE EmptyDataDecls        #-}

{-|
Module      : Escrow.Validator
Description : Definition of contract script.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define the boilerplate for compiling the validator.
-}

-- Uncomment this line for validator profiling:
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}

module Escrow.Validator
  ( Escrowing
  , escrowInst
  , escrowValidator
  , escrowAddress
  ) where

import qualified PlutusTx
import           Ledger
import qualified Ledger.Scripts as Scripts
import qualified Ledger.Typed.Scripts as Scripts

import           Escrow.Types
import           Escrow.OnChain

-- | Definition of type family describing which types are used
--   as datum and redeemers.
data Escrowing
instance Scripts.ValidatorTypes Escrowing where
    type instance DatumType Escrowing    = EscrowDatum
    type instance RedeemerType Escrowing = EscrowRedeemer

escrowInst :: Parameter -> Scripts.TypedValidator Escrowing
escrowInst parameter = Scripts.mkTypedValidator @Escrowing
    ($$(PlutusTx.compile [|| mkEscrowValidator ||])
                         `PlutusTx.applyCode`
                         PlutusTx.liftCode parameter
    )
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.mkUntypedValidator @EscrowDatum @EscrowRedeemer

escrowValidator :: Parameter -> Scripts.Validator
escrowValidator = Scripts.validatorScript . escrowInst

escrowAddress :: Parameter -> Ledger.Address
escrowAddress = scriptAddress . escrowValidator
