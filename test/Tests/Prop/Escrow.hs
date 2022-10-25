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
{-# LANGUAGE RankNTypes          #-}

module Tests.Prop.Escrow (propEscrow, noLockedPayments, checkLastDatum) where

import Control.Monad
import Control.Lens hiding (elements)
import Data.Data
import Data.List (sort)
import Data.Map qualified as Map
import Data.Monoid (Last (..))
import Data.Text hiding (last, filter, singleton, head, length)
import Test.QuickCheck ( Property
                       , shrink, oneof, elements, chooseInteger, tabulate
                       )

import Ledger.Value qualified as Value
import Ledger hiding (singleton)
import Ledger.Ada (lovelaceValueOf)
import Plutus.Contract
import Plutus.Contract.Test ( CheckOptions, defaultCheckOptions, emulatorConfig
                            , mockWalletPaymentPubKeyHash, w1, w2, w3, w4
                            , Wallet, TracePredicate, assertBlockchain
                            )
import Plutus.Contract.Test.ContractModel qualified as CM
import Plutus.Trace.Emulator qualified as Trace
import Plutus.V1.Ledger.Value (geq, valueOf)
import PlutusTx.List qualified as PTx
import PlutusTx.AssocMap qualified as PTx
import PlutusTx hiding (Data)

import Escrow
import Utils.OffChain
import Tests.Utility

-- | Harcoded AssetClass to be used in the contract parameters.
nftAssetClass :: Value.AssetClass
nftAssetClass =
    Value.AssetClass ( "ba0fbe307800337c700982169ca4717ad16216b2d0615ead77a53cc0"
                     , "escrowToken"
                     )

-- | Harcoded contract parameters.
param :: Parameter
param = mkParameter nftAssetClass

-- | Harcoded UTxO that will be used to start the contract.
runUtxo :: TxOutRef
runUtxo = TxOutRef
          "f1520257b50b033e631994827f6a432dad41fdb23b94a55f8c79aeb04a2bb8eb"
          50

-- | Config the checkOptions to use the same emulator config as the Offchain traces.
options :: CheckOptions
options = defaultCheckOptions & emulatorConfig .~ emCfg

-- | Wallets that will be used to test the endpoints
wallets :: [Wallet]
wallets = [w1, w2, w3, w4]

shrinkWallet :: Wallet -> [Wallet]
shrinkWallet w = [w' | w' <- wallets, w' < w]

mockPKH :: Wallet -> PaymentPubKeyHash
mockPKH = mockWalletPaymentPubKeyHash

type DatumSpec = Map.Map Wallet Integer

data EscrowModel = EscrowModel { _isStarted :: Bool, _users :: DatumSpec }
    deriving (Show, Eq, Data)

makeLenses 'EscrowModel

deriving instance Eq   (CM.ContractInstanceKey EscrowModel w s e params)
deriving instance Show (CM.ContractInstanceKey EscrowModel w s e params)

instance CM.ContractModel EscrowModel where

    data Action EscrowModel = Start
                            | AddPayment Wallet Wallet Integer
                            | Collect Wallet
                            | CheckDatum
        deriving (Eq, Show, Data)

    data ContractInstanceKey EscrowModel w s e params where
        OwnerH :: CM.ContractInstanceKey EscrowModel (Last Parameter) EmptySchema Text ()
        UserH  :: Wallet -> CM.ContractInstanceKey EscrowModel () EscrowSchema Text ()
        CheckH :: CM.ContractInstanceKey EscrowModel () TestSchema Text ()

    initialInstances = [CM.StartContract (UserH w) () | w <- wallets]

    initialState = EscrowModel { _isStarted = False
                               , _users     = Map.empty
                               }

    startInstances _ Start = [CM.StartContract OwnerH ()]
    startInstances _ CheckDatum = [CM.StartContract CheckH ()]
    startInstances _ _     = []

    instanceWallet OwnerH = w1
    instanceWallet CheckH = w1
    instanceWallet (UserH w) = w

    instanceContract _ OwnerH _     = run runUtxo
    instanceContract _ CheckH _     = checkEndpoint param
    instanceContract _ (UserH _) _  = endpoints param

    arbitraryAction s
        | started && not (Map.null currentUsers) = oneof [ genAddPayment
                                                         , genCollect
                                                         ]
        | started && Map.null currentUsers = genAddPayment
        | otherwise = pure Start
      where
        started = s ^. CM.contractState . isStarted
        currentUsers = s ^. CM.contractState . users
        lovelaces = 1_000_000
        genAddPayment = AddPayment <$> genWallet <*> genWallet <*> genPayment
        genCollect = Collect <$> genValidWallet
        genPayment = chooseInteger (2 * lovelaces, 20 * lovelaces)
        genWallet = elements wallets
        genValidWallet = elements$ Map.keys currentUsers

    precondition s Start = not $ s ^. CM.contractState . isStarted
    precondition s CheckDatum = s ^. CM.contractState . isStarted
    precondition s (AddPayment _ _ v) =
        s ^. CM.contractState . isStarted && lovelaceValueOf v `geq` minAda
    precondition s (Collect w) =
        s ^. CM.contractState . isStarted && Map.member w currentUsers
      where
        currentUsers = s ^. CM.contractState . users

    nextState Start = do
        isStarted .= True
        users .= Map.empty
        CM.withdraw w1 minAda
        CM.wait 3
    nextState CheckDatum = CM.wait 2
    nextState (AddPayment wFrom wTo v) = do
        currentUsers <- CM.viewContractState users
        CM.withdraw wFrom (lovelaceValueOf v)
        users .= Map.insertWith (+) wTo v currentUsers
        CM.wait 2
    nextState (Collect w) = do
        currentUsers <- CM.viewContractState users
        CM.deposit w (lovelaceValueOf $ Map.findWithDefault 0 w currentUsers)
        users .= Map.delete w currentUsers
        CM.wait 2

    perform _ _ _ Start = CM.delay 3
    perform h _ s CheckDatum = do
        Trace.callEndpoint @"check" (h CheckH) (mapKeysAM mockPKH (s ^. CM.contractState . users))
        CM.delay 2
    perform h _ _ (AddPayment wFrom wTo v) = do
        Trace.callEndpoint @"addPayment" (h $ UserH wFrom) (mockPKH wTo, v)
        CM.delay 2
    perform h _ _ (Collect w) = do
        Trace.callEndpoint @"collect" (h $ UserH w) (mockPKH w)
        CM.delay 2

    shrinkAction _ Start = []
    shrinkAction _ (AddPayment wFrom wTo v) =
           [AddPayment wFrom wTo v' | v' <- shrink v]
        ++ [AddPayment wFrom wTo' v | wTo' <- shrinkWallet wTo]
        ++ [AddPayment wFrom' wTo v | wFrom' <- shrinkWallet wFrom]
    shrinkAction _ (Collect w) = [Collect w' | w' <- shrinkWallet w]
    shrinkAction _ CheckDatum = []

    monitoring _ (AddPayment wFrom _ _) =
        tabulate "AddPayment From Wallets" [show wFrom]
    monitoring _ _ = id

propEscrow :: CM.Actions EscrowModel -> Property
propEscrow = CM.propRunActionsWithOptions options CM.defaultCoverageOptions
             (\ _ -> pure True)

-- # Checking utxo/datum properties using Trace Predicates.
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
               m2 = sort $ (Map.toList . Map.mapKeys mockPKH)
                           (s ^. (CM.contractState . users))
               m1 = sort $ PTx.toList
                   ((eState $ unsafeFromBuiltinData (getDatum eDatum)) :: EscrowState)
            in m1 == m2

    valueContainsNFT :: Value -> Bool
    valueContainsNFT v = (uncurry $ valueOf v) (Value.unAssetClass nftAssetClass) == 1

-- # Checking utxo/datum properties using the Contract Monad.
type TestSchema = Endpoint "check" EscrowState

checkEndpoint
    :: Parameter
    -> Contract () TestSchema Text ()
checkEndpoint p = forever $ handleError logError $ awaitPromise checkEp
  where
    checkEp :: Promise () TestSchema Text ()
    checkEp = endpoint @"check" $ checkOp p

checkOp
    :: forall s
    .  Parameter
    -> EscrowState
    -> Contract () s Text ()
checkOp p dSpec = do
    (_,outxo) <- lookupScriptUtxo (escrowAddress p) nftAssetClass
    datum        <- getContractDatum outxo
    let m2 = sort $ PTx.toList dSpec
        m1 = sort $ PTx.toList $ eState datum
    unless (m1 == m2) $
        error $ Prelude.unwords [ "Missmatch datums, expecting"
                                , show dSpec
                                , "but got"
                                , show $ eState datum
                                ]

specToDatum :: DatumSpec -> EscrowState
specToDatum =  mapKeysAM mockWalletPaymentPubKeyHash

{-# INLINABLE mapKeysAM #-}
mapKeysAM :: (k1 -> k2) -> Map.Map k1 a -> PTx.Map k2 a
mapKeysAM f = PTx.fromList . PTx.map fFirst . Map.toList
    where fFirst (x,y) = (f x, y)


checkLastDatum :: CM.DL EscrowModel ()
checkLastDatum = do
    CM.action Start
    CM.anyActions_
    CM.action CheckDatum


noLockedPayments :: CM.DL EscrowModel ()
noLockedPayments = do
    CM.action Start
    CM.anyActions_
    currentUsers <- CM.viewContractState users
    CM.monitor (tabulate "Collecting payment" [show (Map.size currentUsers) ++ " wallets"])
    mapM_ (CM.action . Collect) (Map.keys currentUsers)
    newUsers <- CM.viewContractState users
    CM.assertModel "Locked Payments should be empty" (const (Map.null newUsers))
