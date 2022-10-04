{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE NumericUnderscores    #-}

{-|
Module      : Tests.OffChain.Tests
Description : Definition of contract script.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : sos@joinplank.com
Stability   : develop
-}

module Tests.OffChain.Tests where

import           Data.Default              (Default (..))
import           Control.Lens

import           Plutus.Contract.Test
import           Plutus.Trace.Emulator     as Emulator

import           Test.Tasty

import           Tests.Utility
import           Tests.OffChain.Traces
import qualified Plutus.V1.Ledger.Ada as Ada

-- | Function that runs a trace and prints the chain
runTrace :: EmulatorTrace () -> IO ()
runTrace = runEmulatorTraceIO' def emCfg

-- | Test of balances and functionality of the traces
tests :: TestTree
tests = testGroup "Escrow Offchain tests" [testFunctionality, testBalance]

-- | Function to build tests that checks
--   if a trace run succesfully.
testRunSucc :: String -> EmulatorTrace () ->  TestTree
testRunSucc s = checkPredicateOptions
                (defaultCheckOptions & emulatorConfig .~ emCfg)
                s
                assertNoFailedTransactions

-- | Function to build tests that checks the balances
--   after running a trace
traceBalances :: String -> EmulatorTrace () -> TracePredicate -> TestTree
traceBalances tname trace tp = checkPredicateOptions
                               (defaultCheckOptions & emulatorConfig .~ emCfg)
                               tname
                               tp
                               trace

-- | TestGroup of funcionality tests
testFunctionality :: TestTree
testFunctionality = testGroup "Testing basic functionality"
    [ testRunSucc "successful trace 1" runTraceSucc
    , testRunSucc "succesfull trace 2" otherSuccTrace
    , testRunSucc "failed collection trace" collectTraceFail
    , testRunSucc "failed addPayment trace" addPayTraceFail
    , testRunSucc "failed over spending trace" overSpendingTraceFail
    ]

-- | TestGroup of balance tests.
testBalance :: TestTree
testBalance = testGroup "Testing Balances of the traces"
    [ traceBalances "succesful trace 1" runTraceSucc succTraceBalance
    , traceBalances "collect trace fail"collectTraceFail collectFailBalance
    , traceBalances "addPayment trace fail" addPayTraceFail addPayFailBalance
    , traceBalances "overSpending trace fail" overSpendingTraceFail overSpendingBalance
    , traceBalances "succesful trace 2" otherSuccTrace otherSuccTraceBalance
    ]

-- | Expected final balances for succesful Trace 1.

-- | Wallet 1 pays the minAda to start the contract.
--   Wallet 2 receives a payment of 2 adas from Wallet 3.
succTraceBalance :: TracePredicate
succTraceBalance =
         walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-2_000_000))
    .&&. walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 2_000_000)
    .&&. walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf (-2_000_000))

-- | Wallet 1 pays minAda to start the contract, then submits an addPayment
--   for wallet 3 of 10 Ada , but it's never collected.
collectFailBalance :: TracePredicate
collectFailBalance =
         walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-12_000_000))
    .&&. walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 0)
    .&&. walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf 0)

-- | Wallet 1 only pays minAda to start the contract.
--   Other balances didn't change because Wallet 1 tries to pay with invalid
--   amounts.
addPayFailBalance :: TracePredicate
addPayFailBalance =
         walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-2_000_000))

-- | Wallet 1 pays minAda to start the contract.
overSpendingBalance :: TracePredicate
overSpendingBalance =
         walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-2_000_000))

-- | Balances of succesful trace 2.
--   Wallet 1 starts the contract paying minAda but also receives a payment of 1 Ada.
--   Wallet 2 receives a payment of 2 adas and pays 1 ada to Wallet 1 and 1 ada to Wallet 3.
--   Wallet 3 pays 2 ada to Wallet 2 and receives a payment of 1 ada of Wallet 2.
otherSuccTraceBalance :: TracePredicate
otherSuccTraceBalance =
         walletFundsChange (knownWallet 1) (Ada.lovelaceValueOf (-1_000_000))
    .&&. walletFundsChange (knownWallet 2) (Ada.lovelaceValueOf 0)
    .&&. walletFundsChange (knownWallet 3) (Ada.lovelaceValueOf (-1_000_000))
