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
import Plutus.Contract.Test (CheckOptions, defaultCheckOptions, emulatorConfig, mockWalletPaymentPubKeyHash, w1, w2, w3, w4, Wallet, TracePredicate, assertBlockchain)
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Ada qualified as Ada
-- import Plutus.V1.Ledger.Contexts (TxOutRef(..))
import Plutus.V1.Ledger.Value (geq, valueOf)
import Ledger hiding (singleton)

import Control.Lens hiding (elements)
import Data.Data
import Data.List (sort)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text hiding (last, filter, singleton, head, length)
import Test.QuickCheck (Property, shrink, choose, oneof, elements)

import Escrow
import Tests.Utility

import qualified PlutusTx.AssocMap as AM
import Escrow.Business()
import Escrow.Types()
import PlutusTx hiding (Data)
import Ledger.Scripts

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
                            | Collect Wallet
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
        | started && not (Map.null currentUsers) = oneof [genAddPayment, genCollect]
        | started && Map.null currentUsers = genAddPayment
        | otherwise = pure Start
      where
        started = s ^. CM.contractState . isStarted
        currentUsers = s ^. CM.contractState . users
        lovelaces = 1_000_000
        genAddPayment = AddPayment <$> genWallet <*> genWallet <*> genPayment
        genCollect = Collect <$> genValidWallet
        genPayment = choose (2 * lovelaces, 20 * lovelaces)
        genWallet = elements wallets
        genValidWallet = elements $ Map.keys currentUsers

    precondition s Start = not $ s ^. CM.contractState . isStarted
    precondition s (AddPayment _ _ v) = s ^. CM.contractState . isStarted &&
                                      Ada.lovelaceValueOf v `geq` minAda
    precondition s (Collect w) = s ^. CM.contractState . isStarted && Map.member w currentUsers
      where
        currentUsers = s ^. CM.contractState . users

    nextState Start = do
        isStarted .= True
        CM.withdraw w1 minAda
        CM.wait 3
    nextState (AddPayment wFrom wTo v) = do
        currentUsers <- CM.viewContractState users
        CM.withdraw wFrom (Ada.lovelaceValueOf v)
        users .= Map.insertWith (+) wTo v currentUsers
        CM.wait 2
    nextState (Collect w) = do
        currentUsers <- CM.viewContractState users
        CM.deposit w (Ada.lovelaceValueOf $ Map.findWithDefault 0 w currentUsers)
        users .= Map.delete w currentUsers
        CM.wait 2

    perform _ _ _ Start = CM.delay 3
    perform h _ _ (AddPayment wFrom wTo v) = do
        Trace.callEndpoint @"addPayment" (h $ UserH wFrom) (mockWalletPaymentPubKeyHash wTo, v)
        CM.delay 2
    perform h _ _ (Collect w) = do
        Trace.callEndpoint @"collect" (h $ UserH w) (mockWalletPaymentPubKeyHash w)
        CM.delay 2

    shrinkAction _ Start = []
    shrinkAction _ (AddPayment wFrom wTo v) = [AddPayment wFrom wTo v' | v' <- shrink v]
                                           ++ [AddPayment wFrom wTo' v | wTo' <- shrinkWallet wTo]
                                           ++ [AddPayment wFrom' wTo v | wFrom' <- shrinkWallet wFrom]
    shrinkAction _ (Collect w) = [Collect w' | w' <- shrinkWallet w]

propEscrow :: CM.Actions EscrowModel -> Property
propEscrow = CM.propRunActionsWithOptions options CM.defaultCoverageOptions
    datumAssertion

datumAssertion :: CM.ModelState EscrowModel -> TracePredicate
datumAssertion s = assertBlockchain f
  where
    f :: [Block] -> Bool
    f ([]:xs) = f xs
    f [[_]] = True
    f bs = let Valid tx = last $ head bs
               outs = txOutputs $ _emulatorTx tx
               dataMap = txData $ _emulatorTx tx
               [utxo] = filter (valueContainsNFT . txOutValue) outs
               Just datumH = txOutDatumHash utxo
               Just eDatum = Map.lookup datumH dataMap
               m2 = sort $ (Map.toList . Map.mapKeys mockWalletPaymentPubKeyHash) (s ^. (CM.contractState . users))
               m1 = sort $ AM.toList ((eState $ unsafeFromBuiltinData (getDatum eDatum)) :: EscrowState)
            in m1 == m2

    valueContainsNFT :: Value -> Bool
    valueContainsNFT v = (uncurry $ valueOf v) (Value.unAssetClass nftAssetClass) == 1

