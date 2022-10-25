{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE OverloadedStrings     #-}

{-|
Module      : Escrow.Types
Description : data types for Escrow Contract.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define the main types used in the contract.
It consists of a Parameter, Escrow Datum and Escrow Redeemer.
-}

module Escrow.Types where

import           Data.Aeson       (FromJSON, ToJSON)
import           GHC.Generics     (Generic)
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup(..), unless, mapM)
import           Ledger           hiding (singleton)
import qualified Ledger.Ada        as Ada

import qualified Prelude          as HP

import           Escrow.Business

-- | Contract Parameter
--   The NFT identifying the unique utxo where the contract state is located.
newtype Parameter = Parameter { stateNFT  :: AssetClass }
    deriving (Generic, HP.Show, HP.Eq, HP.Ord)
    deriving anyclass (FromJSON, ToJSON)

instance Eq Parameter where
  {-# INLINABLE (==) #-}
  p1 == p2 = stateNFT p1  == stateNFT p2

-- | Parameter constructor
mkParameter :: AssetClass -> Parameter
mkParameter ac = Parameter { stateNFT  = ac }

-- | Escrow datum. It contains the state of the contract, stored in a utxo.
newtype EscrowDatum = EscrowDatum { eState :: EscrowState }
    deriving (Generic, HP.Show, HP.Eq)
    deriving anyclass (FromJSON, ToJSON)

instance Eq EscrowDatum where
  {-# INLINABLE (==) #-}
  ec1 == ec2 = eState ec1 == eState ec2

-- | Escrow Datum smart constructor
{-# INLINABLE mkEscrowDatum #-}
mkEscrowDatum :: EscrowState -> EscrowDatum
mkEscrowDatum st = EscrowDatum { eState = st }

-- | Token name for the contract NFT.
escrowTokenName :: TokenName
escrowTokenName = "escrowToken"

-- | Escrow Redeemer. Necessary for knowing which operation is being validated
--   when a script-utxo is spent.
data EscrowRedeemer = AddPayRedeemer PaymentPubKeyHash Integer
                    | CollectRedeemer
    deriving (Generic, HP.Show)
    deriving anyclass (FromJSON, ToJSON)

-- | Smart constructor for the addPayment redeemer.
addPayRedeemer :: PaymentPubKeyHash -> Integer -> Redeemer
addPayRedeemer pkh m = Redeemer $ PlutusTx.toBuiltinData (AddPayRedeemer pkh m)

-- | Smart constructor for the collect redeemer.
collectRedeemer :: Redeemer
collectRedeemer = Redeemer $ PlutusTx.toBuiltinData CollectRedeemer

-- | Minimum amount of ADAs that every UTxO must have
{-# INLINABLE minAda #-}
minAda :: Value
minAda = Ada.toValue Ledger.minAdaTxOut

PlutusTx.makeLift ''Parameter
PlutusTx.makeIsDataIndexed ''EscrowDatum    [ ('EscrowDatum, 0) ]
PlutusTx.makeIsDataIndexed ''EscrowRedeemer [ ('AddPayRedeemer, 0)
                                            , ('CollectRedeemer, 1)
                                            ]
