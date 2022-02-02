{-

This contract is mostly the same as the Vesting contract, so I'll only comment the new stuff

-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week03.Parameterized where

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

-- We define the parameter's type
data VestingParam = VestingParam
    { beneficiary :: PaymentPubKeyHash
    , deadline    :: POSIXTime
    } deriving Show

-- IMP: This line creates an instance of Lift for VestingParam that we'll need in typedValidator.
PlutusTx.makeLift ''VestingParam

{-# INLINABLE mkValidator #-}
{-
We pass the VestingParams on top of the Datum, Redeemer, and ScriptContext. So in this
case mkValidator has 4 parameters instead of the usual 3.

Because we provide the beneficiary and deadline with VestingParam, we pass () as Datum
and Redeemer.
-}
mkValidator :: VestingParam -> () -> () -> ScriptContext -> Bool
-- Same as vesting contract exept for the extra parameter.
mkValidator p () () ctx = traceIfFalse "beneficiary's signature missing" signedByBeneficiary &&
                          traceIfFalse "deadline not reached" deadlineReached
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    {- 
    We check if its signed by the beneficiary the same way as before, but using
    p :: VestingParam instead of the Datum.
    -}
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary p

    deadlineReached :: Bool
    -- Same as vesting contract but using p instead of Datum.
    deadlineReached = contains (from $ deadline p) $ txInfoValidRange info

data Vesting
instance Scripts.ValidatorTypes Vesting where
    -- Change DatumType to unit.
    type instance DatumType Vesting = ()
    type instance RedeemerType Vesting = ()

-- typedValidator now takes an extra parameter (VestingParam)
typedValidator :: VestingParam -> Scripts.TypedValidator Vesting
{-
The mkTypedValidator expectd as his first argument the compiled code of something of
type Datum -> Redeemer -> Contract.
And our mkValidator has a signarure of: VestingParam -> Datum -> Redeemer -> Contract.
We need to do something to the VestingParam parameter before passing it to mkTypedValidator.

The first thing that comes to mind is to partially apply p like this:

    $$(PlutusTx.compile [|| mkValidator p ||])

But we have a problem with that. Everything inside the oxford brackets has to be
known at compile time (That's how TemplateHaskell works), and p isn't known until
we provide a value in run time. Luckly, there's a way around it! :D

The first par of the solution is the applyCode function. This funciton takes two
compiled codes and applys the second argument as a parameter of the first argument.
Now we can compile mkValidator and p separatedly and combine them later. But we need
something else because we still don't know p at compile time.

The second part of the solution comes from the liftCode function.
p is not some arbytrary Haskell code, it's data that doesn't contain any function types.
And for those types, the liftCode function that can compile to Plutus Core at run time!
(To see which types liftCode accepts, see the PlutusTx.Lift class.)

By compiling p at runtime using liftCode and appying the compiled p to the compiled
mkValidator using applyCode, we solved teh issue.
-}
typedValidator p = Scripts.mkTypedValidator @Vesting
    ($$(PlutusTx.compile [|| mkValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @()

-- validator now takes a parameter of VestingParam type
validator :: VestingParam -> Validator
validator = Scripts.validatorScript . typedValidator
-- This is same as: validator p = Scripts.validatorScript $ typedValidator p

-- valHash now takes a parameter of VestingParam type
valHash :: VestingParam -> Ledger.ValidatorHash
valHash = Scripts.validatorHash . typedValidator

-- scrAddress now takes a parameter of VestingParam type
scrAddress :: VestingParam -> Ledger.Address
scrAddress = scriptAddress . validator

-- ####################### START OF THE OFF-CHAIN CODE ##########################

data GiveParams = GiveParams
    { gpBeneficiary :: !PaymentPubKeyHash
    , gpDeadline    :: !POSIXTime
    , gpAmount      :: !Integer
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type VestingSchema =
            Endpoint "give" GiveParams
            {-
            The "grab" endpoint now has to provide the beneficiary and the deadline
            because we no longer have that data in the datum. The beneficiary public
            key will be the publik key of the transaction signer, so we don't have to
            pass it as parameter. But we do need to pass the deadline as a parameter of
            POSIXTime type. We need those two values to filter the utxos.
            -}
        .\/ Endpoint "grab" POSIXTime

give :: AsContractError e => GiveParams -> Contract w s e ()
give gp = do
    -- We create p
    let p  = VestingParam
                { beneficiary = gpBeneficiary gp
                , deadline    = gpDeadline gp
                }
        tx = Constraints.mustPayToTheScript () $ Ada.lovelaceValueOf $ gpAmount gp
    -- typedValidator now needs the parameter p :: VesingParam
    ledgerTx <- submitTxConstraints (typedValidator p) tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace to %s with deadline %s"
        (gpAmount gp)
        (show $ gpBeneficiary gp)
        (show $ gpDeadline gp)

-- Like we said before, we need to pass the deadline d :: POSIXTime to grab
grab :: forall w s e. AsContractError e => POSIXTime -> Contract w s e ()
grab d = do
    now   <- currentTime
    pkh   <- ownPaymentPubKeyHash
    -- Check if the deadline passed
    if now < d
        then logInfo @String $ "too early"
        else do
            let p = VestingParam
                        { beneficiary = pkh
                        , deadline    = d
                        }
            {- 
            Gett all the utxos sitting on the script address that are for us (pkh inside p).
            scrAddress is not a constant anymore, we have to pass p.

            IMP: Because we specify the deadline we ar looking for as an argument, we won't be able
            IMP: to collect the gifts of prevoius deadlines (this didn't happened in the Vesting contract).
            -}
            utxos <- utxosAt $ scrAddress p
            -- If utxos is not empty, all of them are valid available gifts.
            if Map.null utxos
                then logInfo @String $ "no gifts available"
                else do
                    -- We create and submit the transaction for all utxo inside utxos
                    let orefs   = fst <$> Map.toList utxos
                        lookups = Constraints.unspentOutputs utxos      <>
                                  -- Validator needs p
                                  Constraints.otherScript (validator p)
                        tx :: TxConstraints Void Void
                        tx      = mconcat [Constraints.mustSpendScriptOutput oref unitRedeemer | oref <- orefs] <>
                                  Constraints.mustValidateIn (from now)
                    ledgerTx <- submitTxConstraintsWith @Void lookups tx
                    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                    logInfo @String $ "collected gifts"

endpoints :: Contract () VestingSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    -- grab takes a parameter, so it's not a constant anymore.
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''VestingSchema

mkKnownCurrencies []
