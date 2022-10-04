{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-|
Module      : Escrow.Business
Description : Business logic for escrow contract (congestive version).
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : sos@joinplank.com
Stability   : develop

We define in this module any business logic of the contract. This is
the core part in which any modification of the contract state is defined.
This module will be shared between offchain and onchain code, so it must use
the plutus prelude, instead of the haskell prelude.
-}

module Escrow.Business (
    -- ^ Contract State Type
    EscrowState
    -- ^ Smart constructors
  , initialEscrowState
    -- ^ EscrowState operations
  , receivers
    -- ^ Operations
  , addPayment
  , collect
  ) where


import           Data.Maybe

import qualified PlutusTx.Prelude  as Prelude
import qualified PlutusTx.AssocMap as AM
import           PlutusTx.Prelude  hiding (filter, head)
import           Ledger            (PaymentPubKeyHash)

type EscrowState = AM.Map PaymentPubKeyHash Integer

-- | Escrow State initialization
{-# INLINABLE initialEscrowState #-}
initialEscrowState :: EscrowState
initialEscrowState = AM.empty

{-# INLINABLE retrieveVal #-}
retrieveVal :: PaymentPubKeyHash -> EscrowState -> Integer
retrieveVal pkh st = Prelude.fromMaybe 0 $ AM.lookup pkh st

-- | Returns the PaymentPubKeyHash of
--   all the possible receivers in the EscrowState
{-# INLINABLE receivers #-}
receivers :: EscrowState -> [PaymentPubKeyHash]
receivers = AM.keys

-- | Business function for addPayment operation
{-# INLINABLE addPayment #-}
addPayment :: EscrowState -> PaymentPubKeyHash -> Integer -> Maybe EscrowState
addPayment st pkh m
    | m <= 0    = Nothing
    | otherwise = Just $ AM.insert pkh (m + previousVal) st
  where
    previousVal :: Integer
    previousVal = retrieveVal pkh st

-- | Business function for collect operation
{-# INLINABLE collect #-}
collect :: EscrowState -> PaymentPubKeyHash -> Maybe (EscrowState, Integer)
collect st pkh
    | AM.member pkh st = Just (newState, retrieveVal pkh st)
    | otherwise        = Nothing
  where
    newState :: EscrowState
    newState = AM.delete pkh st
