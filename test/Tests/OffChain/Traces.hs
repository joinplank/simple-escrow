{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE OverloadedStrings  #-}

{-|
Module      : Tests.OffChain.Traces
Description : Definition of contract script.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Tests.OffChain.Traces where

import Control.Monad hiding (fmap)

import Plutus.Contract.Test
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Api

import Tests.Utility

import Escrow (run, endpoints)

utxo :: TxOutRef
utxo = TxOutRef
       "2616739c718145f994953df69175f18ad3deef2751fb2572babf2fbe361cd0d6"
       50

utxo1 :: TxOutRef
utxo1 = TxOutRef hardcodedTxHash 5
  where
    hardcodedTxHash :: TxId
    hardcodedTxHash =
        "034cc188fb271a54edb736990966de087e32279015cb46cc7a6ce4597469d0ed"

-- | A trace that runs succesfully and it's balanced.
--   Wallet 1 starts the contract.
--   Wallet 3 adds a payment of 1 Ada for Wallet 2, twice.
--   Wallet 2 collects the payment.
runTraceSucc :: EmulatorTrace ()
runTraceSucc = do
    hRun  <- activateContractWallet (knownWallet 1) (run utxo)
    param <- getParameter hRun
    h2 <- activateContractWallet (knownWallet 2) (endpoints param)
    h3 <- activateContractWallet (knownWallet 3) (endpoints param)

    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h3 ( mockWalletPaymentPubKeyHash (knownWallet 2)
                                  , 1_000_000
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h3 ( mockWalletPaymentPubKeyHash (knownWallet 2)
                                  , 1_000_000
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"collect" h2 $ mockWalletPaymentPubKeyHash (knownWallet 2)
    void $ Emulator.waitNSlots 1

-- | A trace where collect operation is called by a wallet that
--   didn't receive any payments.
--   Wallet 1 starts the contract.
--   Wallet 1 adds a payment of 10 Ada for Wallet 3.
--   Wallet 2 calls collect but fails.
collectTraceFail :: EmulatorTrace ()
collectTraceFail = do
    hRun  <- activateContractWallet (knownWallet 1) (run utxo)
    param <- getParameter hRun
    h1 <- activateContractWallet (knownWallet 1) (endpoints param)
    h2 <- activateContractWallet (knownWallet 2) (endpoints param)

    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h1 ( mockWalletPaymentPubKeyHash (knownWallet 3)
                                  , 10_000_000
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"collect" h2 $ mockWalletPaymentPubKeyHash (knownWallet 2)
    void $ Emulator.waitNSlots 1

-- | A trace where addPayment is called with an invalid amount
--   Wallet 1 starts the contract.
--   Wallet 1 tries to add a payment with 0 Ada but fails.
--   Wallet 1 tries to add a payment with -10 Ada but fails.
addPayTraceFail :: EmulatorTrace ()
addPayTraceFail = do
    hRun <- activateContractWallet (knownWallet 1) (run utxo)
    param <- getParameter hRun
    h1 <- activateContractWallet (knownWallet 1) (endpoints param)

    callEndpoint @"addPayment" h1 ( mockWalletPaymentPubKeyHash (knownWallet 2)
                                  , 0
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h1 ( mockWalletPaymentPubKeyHash (knownWallet 2)
                                  , -10_000_000
                                  )
    void $ Emulator.waitNSlots 1

-- | A trace where a payment is called with an amount greater than the funds
--   of the wallet.
--   Wallet 1 starts the contract.
--   Wallet 1 adds a payment of 1100 Ada for Wallet 2, but fails for lack
--   of funds.
--   Wallet 2 calls collect but fails because there's no deposit for it.
overSpendingTraceFail :: EmulatorTrace ()
overSpendingTraceFail = do
    hRun <- activateContractWallet (knownWallet 1) (run utxo)
    param <- getParameter hRun
    h1 <- activateContractWallet (knownWallet 1) (endpoints param)

    h2 <- activateContractWallet (knownWallet 2) (endpoints param)

    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h1 ( mockWalletPaymentPubKeyHash (knownWallet 2)
                                  , 1_100_000_000
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"collect" h2 $ mockWalletPaymentPubKeyHash (knownWallet 2)
    void $ Emulator.waitNSlots 1

-- | Another trace that runs succesfully
--  There are two payments to Wallet 2 and then.
--  Wallet 2 collects the payments and pays to Wallet 1 and 3.
--  Wallet 1 and 3 collects the payments.
otherSuccTrace :: EmulatorTrace ()
otherSuccTrace = do
    hRun <- activateContractWallet (knownWallet 1) (run utxo)
    param <- getParameter hRun
    h1 <- activateContractWallet (knownWallet 1) (endpoints param)
    h2 <- activateContractWallet (knownWallet 2) (endpoints param)
    h3 <- activateContractWallet (knownWallet 3) (endpoints param)

    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h3 ( mockWalletPaymentPubKeyHash (knownWallet 2)
                                  , 1_000_000
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h3 ( mockWalletPaymentPubKeyHash (knownWallet 2)
                                  , 1_000_000
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"collect" h2 $ mockWalletPaymentPubKeyHash (knownWallet 2)
    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h2 ( mockWalletPaymentPubKeyHash (knownWallet 3)
                                  , 1_000_000
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"addPayment" h2 ( mockWalletPaymentPubKeyHash (knownWallet 1)
                                  , 1_000_000
                                  )
    void $ Emulator.waitNSlots 1

    callEndpoint @"collect" h1 $ mockWalletPaymentPubKeyHash (knownWallet 1)
    void $ Emulator.waitNSlots 1

    callEndpoint @"collect" h3 $ mockWalletPaymentPubKeyHash (knownWallet 3)
    void $ Emulator.waitNSlots 1
