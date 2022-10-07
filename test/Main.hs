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

import Tests.OffChain.Tests
import Tests.Prop.Escrow

main :: IO ()
main = do
    setLocaleEncoding utf8
    putStrLn "----- BEGIN QUICKCHECK TESTS -----"
    quickCheck propEscrow
    putStrLn "----- BEGIN OFFCHAIN TESTS -----"
    defaultMain Main.tests

tests :: TestTree
tests = testGroup "Escrow tests" [ escrowOffChainTests ]

escrowOffChainTests :: TestTree
escrowOffChainTests = testGroup "Offchain tests" [ Tests.OffChain.Tests.tests ]
