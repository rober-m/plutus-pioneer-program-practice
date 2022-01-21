
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

{- This "INLINABLE" is needed because we will use something called "Oxford brackets" that we'll explan 
when we'll use them. This line indicates that mkValidator is in-line inside those special brackets.
-}
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
In order to obtain the validator from mkValidator, we have to use the function mkValidatorScript that we imported from Ledger.Scripts.
If we see the signature of this function, we see that it returns a value of type Validator (awesome, that's what we need)
and that it takes a function with the signature of mkValidator, but not directly.
Before it passes through something called CompiledCode:

CompiledCode (BuiltinData -> BuiltinData -> BuiltinData -> ()) -> Validator

That means that we have to comile the code to Plutus script before passing it to mkValidatorScript.
Luckly, we already have a function to do just that: The compile function imported from PlutusTx.compile.
Let's see the signature of compile:

Q (TExp a) -> Q (TExp (CompiledCode a)) 

Ok, translated to something that we can understand, the compile function takes a syntax tree (a representation) of an expression in Haskell
and returns the syntax tree of the same expression but in Plutus core. Ok, cool. But we have the expression. 
How can we convert mkValidator from expression to syntax tree? By using Template Haskell (explain Template Haskell).

By surrounding the expression between special brackets [|| and ||] called "Oxford backets", like this: [|| expression ||]. 
This is called "Quoting", and it will return the underlying syntax tree that defines the expression.

We pass the result of quoting to the compile fuction and it returns another syntax tree but now one that defines the
expression in Plutus core. But wait! mkValidatorScript takes CompiledCode (Plutus core code) as input, not the syntax tree of the compiled code.
We need a way to transform the syntax tree to the code that thtat it defines. Template Haskell to the rescue! Again!

By adding $$ (called "Splice") to the syntax tree, like this: $$(tree) we obtain the Plutus core code like if we would code it ourselves.
It works something like the reverse of the Oxford brackets. Handy right?

Ok, that's it! we've created our first validator! :D
-}
validator = mkValidatorScript $$(PlutusTx.compile [|| mkValidator ||])

{-
Now, let's use the validator to generate the values that we need.

We need to creat the validator hash because we'll need it to refer to the script later.
We creat the signature of valHash that uses the type ValidatorHash from Ledger, and we define valHash
by using the validatorHash funciton from Scripts, that takes a validator and returns a ValidatorHash:

validatorHash :: Validator -> ValidatorHash

The actual value of validatorHash would look something like this:
67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656

-}
valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash validator

{-
We also need the address of the validator on the blockchain. The address can be obtained by passing
the validator to the scriptAddress function that takes a validator and returns an address of type Adress.

A value of type Address would look something like this:
Address {addressCredential = ScriptCredential 67f33146617a5e61936081db3b2117cbf59bd2123748f58ac9678656, addressStakingCredential = Nothing}
-}
scrAddress :: Ledger.Address
scrAddress = scriptAddress validator


{-
CONGRATULATIONS! We wrote the entire on-chain part of the contract!! :D Of course, we need to interact with 
the validator, so we need some off-chain code that a wallet will run to construct the transactions and talk
with our validator. Let's go create the off-chain code!
-}

{-
This type definition defines the endpoints. The endpoints are ways for the user to trigger something.
We will use two end points:
give: Endpoint that a user can interact with to give money to the contract
grab: Endpoint that a user can interact with to grab all the money of the contract.

The give endpoint receives an Integer as parameter to allow the user to select how many Lovelace wants to give.
The grab endpoint has no parameters because we don't need any. We'll grab all the Lovelace from all the EUTxO sitting at this address.
-}
type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" ()

{-
In the give endpoint signature we specify that we pass an Integer and return a Cotract.
Why do we return a Contract? Because.. TODO.

The Contract constructor needs four parameters:
w : TODO
s : Contract schema.
e : Error that the contract can produce.
a : Type of the value that the contract can produce.

TODO: explain Contract (inside http://0.0.0.0:8002/haddock/plutus-contract/html/Plutus-Contract.html) and AsContractError

-}
give :: AsContractError e => Integer -> Contract w s e ()
{-
Now we'll define the give endpoint.

The give endpoint, first has to create/build the transaction. We'll create the transaction using the
submitTx function. Let's see the signature of submitTx to know what we need to provide:

AsContractError e => TxConstraints Void Void -> Contract w s e CardanoTx

There are two new things here: TxConstraints, Void, and CardanoTx. Let's check them!

Before calling the function, well specify the transaction constraints and save them in a variable called tx:

let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount

-}
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
