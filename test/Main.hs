module Main ( main ) where

import GHC.IO.Encoding
import Test.Tasty

import Tests.OffChain.Tests

main :: IO ()
main = do
    setLocaleEncoding utf8
    defaultMain Main.tests

tests :: TestTree
tests = testGroup "Escrow tests" [ standardOffChainTests ]

standardOffChainTests :: TestTree
standardOffChainTests = testGroup "Offchain tests" [ Tests.OffChain.Tests.tests ]
