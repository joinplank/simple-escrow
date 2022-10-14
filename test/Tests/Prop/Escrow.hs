{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE DataKinds           #-}

module Tests.Prop.Escrow (propEscrow) where 

import Ledger.Value qualified as Value
import Plutus.Contract.Schema (EmptySchema)
import Plutus.Contract.Test (CheckOptions, defaultCheckOptions, emulatorConfig, mockWalletPaymentPubKeyHash, w1, w2, w3, w4, Wallet)
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada qualified as Ada
import Plutus.V1.Ledger.Value (geq)
import Ledger

import Control.Lens
import Data.Data
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text hiding (last, filter)
import Test.QuickCheck (Property, shrink, choose, oneof)

import Escrow
import Tests.Utility

-- | Harcoded AssetClass to be used in the contract parameters.
nftAssetClass :: Value.AssetClass
nftAssetClass = Value.AssetClass ("5546c86bc32890de08fefb67e9b3276343881aebf047b346dfc1aeb7", "escrowToken")

-- | Harcoded contract parameters.
param :: Parameter
param = mkParameter nftAssetClass

-- | Harcoded UTxO that will be used to start the contract.
runUtxo :: TxOutRef
runUtxo = TxOutRef
       "2616739c718145f994953df69175f18ad3deef2751fb2572babf2fbe361cd0d6"
       50

-- | Config the checkOptions to use the same emulator config as the Offchain traces.
options :: CheckOptions
options = defaultCheckOptions & emulatorConfig .~ emCfg


-- | Wallets that will be used to test the endpoints
wallets :: [Wallet]
wallets = [w1, w2, w3, w4]

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

data EscrowModel = EscrowModel { _isStarted :: Bool, _users :: Map.Map Wallet Integer}
    deriving (Show, Eq, Data)

makeLenses 'EscrowModel

deriving instance Eq (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

instance CM.ContractModel EscrowModel where

    data Action EscrowModel = Start 
                            | AddPayment Wallet Wallet Integer
        deriving (Eq, Show, Data)

    data ContractInstanceKey EscrowModel w s e params where
        OwnerH :: CM.ContractInstanceKey EscrowModel (Last Parameter) EmptySchema Text ()
        UserH  :: Wallet -> CM.ContractInstanceKey EscrowModel () EscrowSchema Text ()
    
    initialInstances = [CM.StartContract (UserH w) () | w <- wallets] 

    initialState = EscrowModel { _isStarted = False
                               , _users     = Map.empty }

    startInstances _ Start = [CM.StartContract OwnerH ()]
    startInstances _ _     = []

    instanceWallet OwnerH = w1
    instanceWallet (UserH w) = w

    instanceContract _ OwnerH _ = run runUtxo
    instanceContract _ (UserH _) _  = endpoints param

    arbitraryAction s 
        | started = do
            oneof [AddPayment wFrom wTo <$> choose (2 * lovelaces, 20 * lovelaces) 
                  | wFrom <- wallets, wTo <- wallets]
        | otherwise = pure Start
      where
        started = s ^. CM.contractState . isStarted
        lovelaces = 1_000_000

    precondition s Start = not $ s ^. CM.contractState . isStarted
    precondition s (AddPayment _ _ v) = s ^. CM.contractState . isStarted &&
                                      Ada.lovelaceValueOf v `geq` minAda

    nextState Start = do
        isStarted .= True
        CM.withdraw w1 minAda
        CM.wait 3
    nextState (AddPayment wFrom wTo v) = do
        currentUsers <- CM.viewContractState users
        CM.withdraw wFrom (Ada.lovelaceValueOf v)
        users .= Map.insertWith (+) wTo v currentUsers
        CM.wait 2

    perform _ _ _ Start = CM.delay 3
    perform h _ _ (AddPayment wFrom wTo v) = do
        Trace.callEndpoint @"addPayment" (h $ UserH wFrom) (mockWalletPaymentPubKeyHash wTo, v)
        CM.delay 2

    shrinkAction _ Start = []
    shrinkAction _ (AddPayment wFrom wTo v) = [AddPayment wFrom wTo v' | v' <- shrink v] 
                                           ++ [AddPayment wFrom wTo' v | wTo' <- shrinkWallet wTo] 
                                           ++ [AddPayment wFrom' wTo v | wFrom' <- shrinkWallet wFrom]

propEscrow :: CM.Actions EscrowModel -> Property
propEscrow = CM.propRunActionsWithOptions options CM.defaultCoverageOptions 
    (\ _ -> pure True)
