{-
I'll only comment the new stuff, everything else, look for in the gift, FortyTwo, 
Typed, and isData contracts of Week02.
-}
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

module Week03.Vesting where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           GHC.Generics         (Generic)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   (TxConstraints)
import qualified Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), Show (..), String)
import           Text.Printf          (printf)

{-
We create the Datum type VestingDatum that contains the beneficiary and the deadline that the beneficiary has
unil to collect the Ada.
We derive Show for debugging purposes (so we can display the values on the console if needed).
-}
data VestingDatum = VestingDatum
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

PlutusTx.unstableMakeIsData ''VestingDatum

{-# INLINABLE mkValidator #-}
{-
The beneficiary doesn't need any information to redeem the deposited money because all the
information needed is provided by the Datum, so we pass ().
-}
mkValidator :: VestingDatum -> () -> ScriptContext -> Bool
{-
This time we'll use the ScriptContext.

The validator has to check two things in the transaction that unlocks the funds:
- That it's signed by the beneficiary (signedByBeneficiary :: Bool)
- That the deadline has been reached (deadlineReached :: Bool)

If any of those two conditions are false, the validator rejects the transaction.
-}
mkValidator dat () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                         traceIfFalse "deadline not reached" deadlineReached
  where
    {- 
    info contains all the information about the transaction (inputs, outputs, etc.) 
    that the validator can access.
    -}
    info :: TxInfo
    -- scriptContextTxInfo extracts TxInfo from the ScriptContext record.
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    {-
    To know if the transaction has been signed by the beneficiary, first we'll extract
    the beneficiary from the Datum (beneficiary dat), then we extract the PubKeyHash using
    the unPaymentPubKeyHash record. Now, we have the public key hash of beneficiary, and we
    have the TxInfo of the transaction.
    
    To check if a transaction has been signed by a specifc
    PubKeyHash, we have the helper function txSignedBy. txSigned by takes a Txinfo and a PubKeyHash
    and returns True if TxInfo is signed by PubkeyHash and False otherwise.
    -}
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

    deadlineReached :: Bool
    {-
     To check if a deadline has been reached, we can use the contains function. The contains
    function takes two values of type Interval and returns True if the first Interval is entirely
    contained in the second Interval and False otherwise. By checking that an interval that starts
    at the deadline is contained in the transaction's valid inteval range, we can be sure that the
    deadline has been reached.

    To create the first interval we'll use the from function, that takes a value "a" and produces an
    Interval that includes all values that are greater or equal to "a". passing the deadline of dat
    we obtain an interval that goes from the deadline until the end of times.

    The second interval is already provided by the transaction. We can extract it by using
    txInfoValidRange.
    -}
    deadlineReached = contains (from $ deadline dat) $ txInfoValidRange info

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

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

-- ####################### START OF THE OFF-CHAIN CODE ##########################

{-
All necesary data will be provided by the "give" endpoint. We'll create the type
GiveParams that contains the beneficiary, the deadline, and the ammount of Ada
that the giver wants to give.

The fields are the same as the Datum exept for the gpAmount (ammount of ADA to give).

TODO: Why do we derive Generic, ToJSON, etc.?
-}
data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
        .\/ Endpoint "grab" ()

give :: AsContractError e => GiveParams -> Contract w s e ()
-- We pass gp :: GiveParams as a parameter.
give gp = do
    let dat = VestingDatum
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx  = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ gpAmount gp
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    -- This is why we needed to detive "show" when defining VestingDatum.
    {-
    We have to specify @String (using TypeApplications extension) because now we have
    the extension OverloadedStrings and logInfo has to know which instance of String
    we're providing.
    -}
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    utxos <- Map.filter (isSuitable pkh now) <$> utxosAt scrAddress
    if Map.null utxos
        then logInfo @String $ "no gifts available"
        else do
            let orefs   = fst <$> Map.toList utxos
                lookups = Constraints.unspentOutputs utxos  <>
                          Constraints.otherScript validator
                tx :: TxConstraints Void Void
                -- TODO: How does Plutus know the Datum? Magic on the background? Lars doesn't know (Q&A Week03)
                -- TODO: Lars thinks that the PlutusPlayground automatically includes the Datum.
                tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                          Constraints.mustValidateIn (from now)
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ "collected gifts"
  where
    isSuitable :: PaymentPubKeyHash -> POSIXTime -> ChainIndexTxOut -> Bool
    isSuitable pkh now o = case _ciTxOutDatum o of
        Left _          -> False
        Right (Datum e) -> case PlutusTx.fromBuiltinData e of
            Nothing -> False
            Just d  -> beneficiary d == pkh && deadline d <= now

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
