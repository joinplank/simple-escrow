{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

{-|
Module      : Tests.Utility
Description : Definition of contract script.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : sos@joinplank.com
Stability   : develop
-}

module Tests.Utility where

import           Data.Default               (def)
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)

import           Control.Lens               ((.~))
import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras as Extras

import           Ledger
import           Ledger.Ada                 as Ada
import           Plutus.Contract
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           Prelude                    hiding ((+))

import           Escrow

type EscrowHandle = ContractHandle (Last Parameter) EmptySchema Text

type CEscrowHandle w s = ContractHandle w s Text

runTrace :: EmulatorTrace () -> IO ()
runTrace = Emulator.runEmulatorTraceIO' def emCfg

emCfg :: EmulatorConfig
emCfg = initialChainState .~ Left (Map.fromList wallets) $ def
  where
    wallets :: [(Wallet,Value)]
    wallets = [(w, v) | w <- init knownWallets]

    v :: Value
    v = Ada.lovelaceValueOf  1_000_000_000

getParameter :: EscrowHandle -> EmulatorTrace Parameter
getParameter h = do
    void $ Emulator.waitNSlots 1
    l <- observableState h
    case l of
        Last (Just param) -> Extras.logInfo (show param) >> return param
        Last _            -> Emulator.waitNSlots 1 >> getParameter h
