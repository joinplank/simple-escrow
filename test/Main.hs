{-# LANGUAGE ImportQualifiedPost #-}
{-|
Module      : Main
Description : Definition of contract script.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop
-}

module Main ( main ) where

import GHC.IO.Encoding
import Test.Tasty
import Test.QuickCheck
import Plutus.Contract.Test.ContractModel qualified as CM


import Tests.OffChain.Tests
import Tests.Prop.Escrow

main :: IO ()
main = do
    setLocaleEncoding utf8
    putStrLn "----- BEGIN QUICKCHECK TESTS -----"
    quickCheck $ CM.forAllDL checkLastDatum propEscrow
    putStrLn "----- BEGIN OFFCHAIN TESTS -----"
    defaultMain Main.tests

tests :: TestTree
tests = testGroup "Escrow tests" [ escrowOffChainTests ]

escrowOffChainTests :: TestTree
escrowOffChainTests = testGroup "Offchain tests" [ Tests.OffChain.Tests.tests ]
