{-
This contract is basically the same as the gift/burn contract but with a small change.
I'll only comment the changes, everything else, look for in the gift and
FortyTwo contracts.
-}

{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week02.Typed where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import qualified PlutusTx.Builtins    as Builtins
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)

{-# INLINABLE mkValidator #-}
{-
Having Datum, Redeemer, and Context as BuitinData is usefull for efficient computation,
but having strong types specific for the use-case is much more convenient (and helpful 
to learn Plutus).

Its important to notice that using strong types has an impact on the resources needed to
execute the contract. The compile Plutus code, the number of executions needed, and the
memory consumption, all become bigger. But we'll worry about that once we become skilled
Plutus developers.

In this contract, we don't care about the Datum, so we'll define it as a value of type ().
As a redeemeer, we'll use an Integer. We'll compare if that integer is 42, and if it is, the
validator passes (same as the FortyTwo contract). And finally, we'll use ScriptContext as the
type for the script context value.

The final change is that now the validator returns a Bool (True or False). True if the validation
passes, False or error otherwise.
-}
mkValidator :: () -> Integer -> ScriptContext -> Bool
{-
Like before, we'll only care about the redeemer, so we'll ignore the Datum and ScriptContext.
The logic of mkValidator could be:

    mkValidator _ r _ = r == 42

That would work as espected. But ind the case that we get a script failuer, we wouldn't be 
able to differentiate why, and it would be hard to debug. In this case, it's not really
needed, but it's important for when we have a more complex validation going on.

To solve that, we'll use the traceIfFalse function (traceIfFalse :: BuiltinString -> Bool -> Bool)
that takes the string to show if the Bool passed as a second argument is false, and it 
returns that same Bool.
-}
mkValidator _ r _ = traceIfFalse "wrong redeemer" $ r == 42
{-
Because we now use strong types for the mkValidator parameters, we need to write some more
boilerplate code to be able to compile it to Plutus core.

First, we need to create a new type that encodes the information about the type of
the Datum and the Redeemer.

We'll call it Typed, but you can call it however you want. This type won't have any 
constructors because we'll never need a value of this type. This is only used to
provide the instance of the class Scripts.ValidatorTypes for the Typed type.

In Typed, we record that our DatumType will be of type (), and that our RedeemerType
will be of type Integer.

Note: Now, the scripts come from Sc Ledger.Typed.Scripts so we can use the typed version
of the scripts.
-}
data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    type instance RedeemerType Typed = Integer

{-
To do the actual compilation to Plutus core, we now have to use the mkTypedValidator
function, that takes TODO

The wrapValidator converts the BuiltinData types into the types that we defined.
Then, our custom logic that we defined in mkValidator is executed, and finally,
if the validator returns True we translate it into (), else we translate it
into an exception.

-}
typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @() @Integer

-- We create the validator from typedValidator
validator :: Validator
validator = Scripts.validatorScript typedValidator

-- The validator hash is derived from typedValidator
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

-- The srcAdress is derived from validator, not typedValidator
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    {- 
    For the typed version we use mustPayToTheScript instead of mustPayToOtherScript
    mustPayToTheScript has the advantage that instead of provide the Datum as BuiltinData,
    we can pass the type that we defined earlier (in this case the unit type)
    -}
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    -- We pass typedValidator to submitTxConstraints
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab r = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI r | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
