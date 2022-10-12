{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE FlexibleInstances   #-}

module Tests.Prop.Escrow (propEscrow) where 

import Ledger.Value qualified as Value
import Plutus.Contract.Schema (EmptySchema)
import Plutus.Contract.Test (CheckOptions, defaultCheckOptions, emulatorConfig, w1)
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.V1.Ledger.Contexts (TxOutRef(..))

import Control.Lens
import Data.Data
import Data.Monoid (Last (..))
import Data.Text
import Test.QuickCheck (Property)

import Escrow
import Tests.Utility

-- | Harcoded AssetClass to be used in the contract parameters.
nftAssetClass :: Value.AssetClass
nftAssetClass = Value.AssetClass ("54ace0dc05c546d54ad98777d61bd081c6f69054f9199851f600b709", "escrowToken")

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

newtype EscrowModel = EscrowModel { _isStarted :: Bool }
    deriving (Show, Eq, Data)

makeLenses 'EscrowModel

deriving instance Eq (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

instance CM.ContractModel EscrowModel where

    data Action EscrowModel = Start
        deriving (Eq, Show, Data)

    data ContractInstanceKey EscrowModel w s e params where
        OwnerH :: CM.ContractInstanceKey EscrowModel (Last Parameter) EmptySchema Text ()
    
    initialInstances = []

    initialState = EscrowModel { _isStarted = False }

    startInstances _ Start = [CM.StartContract OwnerH ()]

    instanceWallet OwnerH = w1

    instanceContract _ OwnerH _ = run runUtxo

    arbitraryAction _ = pure Start

    precondition s Start = not $ s ^. CM.contractState . isStarted

    nextState Start = do
        isStarted .= True
        CM.withdraw w1 minAda
        CM.wait 3

    perform _ _ _ Start = CM.delay 3

propEscrow :: CM.Actions EscrowModel -> Property
propEscrow = CM.propRunActionsWithOptions options CM.defaultCoverageOptions 
    (\ _ -> pure True)
