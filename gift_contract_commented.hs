
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week02.Gift where

import           Control.Monad       hiding (fmap)
import           Data.Map            as Map
import           Data.Text           (Text)
import           Data.Void           (Void)
import           Plutus.Contract
import           PlutusTx            (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins   as Builtins
import           PlutusTx.Prelude    hiding (Semigroup(..), unless)
import           Ledger              hiding (singleton)
import           Ledger.Constraints  as Constraints
import qualified Ledger.Scripts      as Scripts
import           Ledger.Ada          as Ada
import           Playground.Contract (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH       (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types    (KnownCurrency (..))
import           Prelude             (IO, Semigroup (..), String)
import           Text.Printf         (printf)

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- INLINABLE needed because we will use it inside Template Haskell, and it needs everything in one line.
{-# INLINABLE mkValidator #-}
{- 

Signature of a function that will make the validator. It takes three parameters:
- Datum: Comes from the output that's being consumed.
- Redeemer: Comes from the input that is consuming.
- Context: Consuming transaction with all its inputs and outputs.

Although we can use specific types for each one, we will use the low-level "BuiltinData" because it's resource efficient and
we won't make a complex validation.

The function wil return () (called "unit", a builtin type in Haskell) that it's similar to the void type in Java, Dart, and other progamming languages.
Tthis types has only one value which is () (also called "unit"). So this type carries no information.
But wait. The validator has no side-effects, and the funtion will always return the same. So, why do we do this? How do I know if the transaction was valid?
Because there's one other thing that the function can do: It can have an error/exception! So that's how we no if the validation passes.
If there's no error, the validation passes. If there's an error, it fails.

At the end, the validator will be a script in the blockchain compiled to Plutus core (on-chain low-level language version of Plutus).
-}
mkValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
{-
In this line, we define the validator funcition.
We will write the most simple validator that we can. One that ignores all inputs (Datum,Redeemer, and Context) and passes the validation.
We ignore parameters by asigning an underscore to them. We have three parameters, so we put three underscores.
Then, in the body of the function (after the =) we return the (). That's it! We have a function that creates a validator that always passes complete!

Obviously, we should write some custom logic to validate the transacition based on whatever we want to check. But that's for a future tutorial.
Now, we want to have a look at the basic patterns and concepts behind a Plutus contract.
-}
mkValidator _ _ _ = ()


{-
Now, we will use mkValidator to create our validator.
validator is of type Validator.

-}
validator :: Validator
{-
In order to obtain the validator, you have to compile the mkValidator function to core Plutus
-}
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Contract w s e ()
grab = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
