{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

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
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator      as Emulator
import           Prelude                    hiding ((+))

import           Escrow
import           Utils.OffChain             (LastTxId)

type EscrowHandle = ContractHandle (LastTxId Parameter) EscrowSchema Text

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
        Last (Just (Right param)) -> Extras.logInfo (show param) >> return param
        Last _                    -> Emulator.waitNSlots 1 >> getParameter h

getMCTxId :: EscrowHandle -> EmulatorTrace TxId
getMCTxId h = do
    void $ Emulator.waitNSlots 1
    l <- observableState h
    case l of
        Last (Just (Left (txid,txv))) -> Extras.logInfo (show (txid, txv)) >>
                                         return txid
        Last _                        -> Emulator.waitNSlots 1 >> getMCTxId h


getLastObsState :: EscrowHandle -> EmulatorTrace ()
getLastObsState h = do
    void $ Emulator.waitNSlots 1
    l <- observableState h
    case l of
        Last (Just (Left mcTxId)) -> Extras.logInfo (show mcTxId)
        Last (Just (Right param)) -> Extras.logInfo (show param)
        _                         -> Emulator.waitNSlots 1 >>
                                        getLastObsState h
