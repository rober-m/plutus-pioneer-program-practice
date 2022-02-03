{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week02.Homework1 where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO)
import qualified Prelude              as P
import           Text.Printf          (printf)

-- Now we have two beneficiaries
data VestingDatum = VestingDatum
    { beneficiary0 :: PaymentPubKeyHash
    , beneficiary1 :: PaymentPubKeyHash
    , deadline     :: POSIXTime
    } deriving P.Show

PlutusTx.unstableMakeIsData ''VestingDatum

-- TODO: Check if my answer works. I tried but I can't grab the gift from any
-- TODO: wallet that isn't the gifter. Even with Lars' code and double checking the PubKey
-- TODO: of the beneficiary. Try again after a few days.

{-# INLINABLE mkValidator #-}
-- This should validate if either beneficiary0 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary1 has signed the transaction and the deadline has passed.
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkValidator d _ ctx = traceIfFalse "Benef 0 can't take money untill deadline is reached" (signedByBenefOne && not deadlineReached) ||
                      traceIfFalse "Benef 1 can't take money after the deadline" (signedByBenefTwo && deadlineReached)
  where
    info = scriptContextTxInfo ctx
    signedByBenefOne = (txSignedBy info $ unPaymentPubKeyHash $ beneficiary1 d)
    signedByBenefTwo = (txSignedBy info $ unPaymentPubKeyHash $ beneficiary2 d)
    deadlineReached  = contains (from $ deadline d) $ txInfoValidRange info

-- ################### IMP: Lars' Solution ######################################

{-
mkValidator dat () ctx
    | (unPaymentPubKeyHash (beneficiary1 dat) `elem` sigs) && (to       (deadline dat) `contains` range) = True
    | (unPaymentPubKeyHash (beneficiary2 dat) `elem` sigs) && (from (1 + deadline dat) `contains` range) = True
    | otherwise                                                                                          = False
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    sigs :: [PubKeyHash]
    sigs = txInfoSignatories info

    range :: POSIXTimeRange
    range = txInfoValidRange info
-}

-- ################### IMP: Solution inspired by Lar's Solution ##################

{-
mkValidator d _ ctx 
    | traceIfFalse "Benef 0 can't take money untill deadline is reached" (signedByBenefOne && not deadlineReached) = True
    | traceIfFalse "Benef 1 can't take money after the deadline" (signedByBenefTwo && deadlineReached)             = True
    | otherwise                                                                                                    = False
    where
      info = scriptContextTxInfo ctx
      signedByBenefOne = (txSignedBy info $ unPaymentPubKeyHash $ beneficiary1 d)
      signedByBenefTwo = (txSignedBy info $ unPaymentPubKeyHash $ beneficiary2 d)
      deadlineReached  = contains (from $ deadline d) $ txInfoValidRange info
      range = txInfoValidRange info
-}

data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance DatumType Vesting = VestingDatum
    type instance RedeemerType Vesting = ()

typedValidator :: Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidator @Vesting
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @VestingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    pkh <- ownPaymentPubKeyHash
    let dat = VestingDatum
                { beneficiary0 = gpBeneficiary gp
                , beneficiary1 = pkh
                , deadline     = gpDeadline gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @P.String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (P.show $ gpBeneficiary gp)
        (P.show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now    <- currentTime
    pkh    <- ownPaymentPubKeyHash
    utxos  <- utxosAt scrAddress
    let utxos0 = Map.filter (isSuitable $ \dat -> beneficiary1 dat == pkh && now <= deadline dat) utxos
        utxos1 = Map.filter (isSuitable $ \dat -> beneficiary2 dat == pkh && now >  deadline dat) utxos
    logInfo @P.String $ printf "found %d gift(s) to grab" (Map.size utxos0 P.+ Map.size utxos2)
    unless (Map.null utxos0) $ do
        let orefs   = fst <$> Map.toList utxos0
            lookups = Constraints.unspentOutputs utxos0 P.<>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] P.<>
                      Constraints.mustValidateIn (to now)
        void $ submitTxConstraintsWith @Void lookups tx
    unless (Map.null utxos1) $ do
        let orefs   = fst <$> Map.toList utxos1
            lookups = Constraints.unspentOutputs utxos1 P.<>
                      Constraints.otherScript validator
            tx :: TxConstraints Void Void
            tx      = mconcat [Constraints.mustSpendScriptOutput oref $ unitRedeemer | oref <- orefs] P.<>
                      Constraints.mustValidateIn (from now)
        void $ submitTxConstraintsWith @Void lookups tx
  where
    isSuitable :: (VestingDatum -> Bool) -> ChainIndexTxOut -> Bool
    isSuitable p o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum d) -> maybe False p $ PlutusTx.fromBuiltinData d

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
