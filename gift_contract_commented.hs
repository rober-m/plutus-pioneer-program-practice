{- 
These are called "Language Pragmas". 
They direct the Haskell compiler to enable an extension or modification of the Haskell 
language.
We won't cover in detail what each pragma does, becauase the tutorial it's long enough
without that. But I'll explain some basic concepts when we need them to understand the code.
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-
For the sake of understanding the code, we can ignore this imports and think of 
them as "importing all the Plutus modules that we'll need". We don't care where
a function is defined, we care about what it does and how can we use it.
-}

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
The .\/ operator is a way to join Endpoints.
-}
type GiftSchema =
	    Endpoint "give" Integer
	.\/ Endpoint "grab" ()

{-
In the give endpoint signature we specify that we pass an Integer and return a Cotract.
Why do we return a Contract? Because.. TODO.

The Contract constructor needs four parameters:
w : TODO
s : Contract schema. It contains effects such as collecting inputs fromusers, watching thelbockchain
for transactions, and producing transactions.
e : Error that the contract can produce.
a : Type of the value that the contract can produce. We'll allways return (), so the type of a will allways be ().

AscontractError e => Indicates that e is restricted to be of type AsContractError.
-}
give :: AsContractError e => Integer -> Contract w s e ()
{-
Now we'll define the give endpoint.

The give endpoint, first has to create/build the transaction. We'll create the transaction using the
submitTx function. Let's see the signature of submitTx to know what we need to provide:

AsContractError e => TxConstraints Void Void -> Contract w s e CardanoTx

We have to provide TxConstraints Void Void and the function will produce a Contract that returns
a value of type CardanoTx.

Ok, let's go in parts. Before calling the function, well specify the transaction constraints and 
save them in a variable called tx:

let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount

The signature of mustPayToOtherScript (defined in Ledger/Constraints/TxConstraints.hs) is:

mustPayToOtherScript :: ValidatorHash -> Datum -> Value -> TxConstraints i o

So, to create the transactions constraints, we have to provide the valHash, the Datum, and the value that we
want to transfer. We already calculated valHash, one value done. And in this case, we don't care about
the Datum (because we'll ignore it in the validator), but we have to specify one, so we'll provide an arbitrary one.

We can create a Datum by using the Datum constructor. Its signature ( Datum :: BuiltinData -> Datum ) indicates 
that we have to pass a value of type BuiltinData. We'll do that by using the Builtins.mkI function 
(mkI :: BuiltinInteger -> BuiltinData) that takes a BuiltinInteger and returns the value of type
BuiltinData. We'll pass a 0 as value, it doesn't really matter.

All the Datum operation together will happen in this expression (Datum $ Builtins.mkI 0).

Now, we need to specify how much funds we want to send in the transaction (the value of type Value).
To obtain the Value in Lovelace, we'll pass the ammount of type Integer to the Ada.lovelaceValueOf function.
This value will be provided by the user that wants to give ada, so it will be a prarameter of the 
give function called amount.

And that's it! As a reward for our efforts, we get the transaction constraints that say to pay "amount" of 
lovelace to valHash script with the Datum we provided.

Well submit the transaction with submitTx and we'll save the transaction of type CardanoTx in ledgerTx.
Now, we have to wait for confirmation by using the awaitConfirmed function 
(awaitTxConfirmed :: AsContractError e => TxId -> Contract w s e ()) that takes a TxId that we can obtain 
by passing ledgerTx to getCardanoTxId (getCardanoTxId :: CardanoTx -> TxId).

Once the transaction is confirmed, we log a message to record the event with logInfo function:

    logInfo @String $ printf "made a gift of %d lovelace" amount

-}
give amount = do
    let tx = mustPayToOtherScript valHash (Datum $ Builtins.mkI 0) $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTx tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

{-
The signature of the grab function is the same as the give function except for the lack of
Integer (because we don't need it. We'll grab everything in the contract) and the 
extra "forall" keyword. That is a keyword provided by the ScopedTypeVariables pragma.
forall is used to bind type variables and refer to them later throught the remainder 
of the function’s definition. For the scope of this tutorial, we can safely
ignore the details.
-}
grab :: forall w s e. AsContractError e => Contract w s e ()
{-
This time, we'll create a more complex transaction, so we'll create a couple of intermediary
variables.

The first thing that we're going to do is to look at all the utxos sitting at the srcAdress 
and saves them in utxos:

    utxos <- utxosAt scrAddress

The, we'll get the references for all those utxos and save them on the orefs variable:

    let orefs   = fst <$> Map.toList utxos

In this line, we pass th utxos to the function Map.toList (toList :: Map k a -> [(k, a)]) that
takes a Map and returns a list of tuples containing each tuple a key-value pair. Then, we'll
apply fst function to each tuplple of the list using <$> to obtain the list of references (k)
of all utxos.

Next, we have to obtain the lookups to tell the wallet how to construct the transaction. We'll do this by
Smashing together the values of type ScriptLookups returned of these functions:

 unspentOutputs: A script lookups value that uses the map of unspent outputs to resolve input constraints.
 otherScript: A script lookups value with a validator script. If we want to consume a utxo sitting at a
		script address, then the spending transaction needs to provide the validator code ( where
		the producing transaction only has to provide the hash).

Like this:

    let lookups = Constraints.unspentOutputs utxos      <>
		  Constraints.otherScript validator

<> means that we're joining/smashing together the two constraints (for more information on the
<> operator, you can see [this video](https://youtu.be/bsp5pJlw6R0)).

Now, let's define the transaction.
Because we now have multiples utxo, we'll make use of list comprehension (if you don't know
how list comprehension works, [here]() is a tweet where do an overview on how it works).

We'll take each utxo reference in orefs and pass it to the mustSpendScriptOutput function.
The mustSpendScriptOutput function signature is

mustSpendScriptOutput:: TxOutRef -> Redeemer -> TxConstraints i o 

So, we need to pass a utxo reference and a Redeemer to obtain the TxConstraints.
We already have the reference (each value inside the orefs list) and because we ignore
the redeemer in the validator, we can pass any arbitrary value of type Redeemer (same as with Datum).

We'll creat a random redeemer same as with Datum:

Redeemer (Builtins.mkI 17)

or the same but with different syntax:
    
Redeemer $ Builtins.mkI 17

And we'll pass each oref with this Redeemer to mustSpendScriptOutput, 
generating a list of transaction constraints (one for each utxo).

With all those operations, we obtained a list of TxConstraints. We'll
execute the transactions using submitTxConstraintsWith, that will build a transaction 
that satisfies the constraints and then submit it to the network using the given constraints.
Its signature is a handfull:

submitTxConstraintsWith :: (FromData (DatumType a), ToData (RedeemerType a), ToData (DatumType a), AsContractError e) => ScriptLookups a -> TxConstraints (RedeemerType a) (DatumType a) -> Contract w s e CardanoTx

The important part for us is that we have all the right types except for TxConstraints. We don't
have a single TxConstraints, we have a list of them! So we need a way to merge them all in a single
TxConstraints. That's when mconcat (mconcat :: Monoid a => [a] -> a) comes in handy. We pass the list
of transaction constraints to mconcat and we obtain a single all-powerfull Txconstraints that we can
pass to submitTxconstrinatsWith. Finally, we save the value in the tx variable:

    tx  = mconcat [mustSpendScriptOutput oref $ Redeemer $ Builtins.mkI 17 | oref <- orefs]

As we can see in the signature submitTxConstraintsWith takes as inputs the script lookkups and
the transaction constraints. So we have everything that we need to execute the transaction and save it
as ledgetTx:

    ledgerTx <- submitTxConstraintsWith @Void lookups tx

The @Type (@Void, @String, etc.) is TypeApplication syntax.
TypeApplication allows us to give explicit type arguments to a polymorphic function.
Here, we're basically saying to Haskell that we want to "pick up the instance of @Void" for the
FromData (DatumType a) constraint (the first constraint before the fat arrow =>).
You can learn more about TypeApplication [here](https://gitlab.haskell.org/ghc/ghc/-/wikis/type-application).

Once the transaction i executed, we wait for the confirmation, like in the give endpoint:

    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

And after confirmation, we log a message confirming that we collected that sweet sweet Lovelace

    logInfo @String $ "collected gifts"
-}
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

{-
Now, we'll put everything together in teh endpoints function.

First we'll create the endpoints by using the endpoint function of signature:

endpoint :: forall l a w s e b. (HasEndpoint l a s, AsContractError e, FromJSON a) => (a -> Contract w s e b) -> Promise w s e b

So, we need to provide a function that returns a Contract.
We already have the contracts defined: give and grab. We'll indicate that the type
of l (the first constraint) is of type "give" and "grab" respectively.
Why? -> TODO.

	give' = endpoint @"give" give
	grab' = endpoint @"grab" $ const grab

We can do two things (two choices), either we give some Ada, or we grab all of it. To
express that choice we use the select function. select returns the contract that makes progress 
first, discarding the other one. We pass the select result to awaitPromise to
wait for the user choice. Then, we use >> and pass the same endpoints function that we're
defining to recursevely call it as many times as the user needs. And that's it!
we have our endponts defined!:
-}

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" $ const grab
{-
We are done! Both our on-chain and off-chain code is finished! 
-}

{-
mkSchemaDefinitions is a boilerplate function that creates the schema of the 
contract. We apply it to use the code on the playground. It's not part of the
contract.
-}
mkSchemaDefinitions ''GiftSchema
{-
we indicate mkKnownCurrencies [] so we can use Ada in the playground. This is 
not part of the contract.
-}
mkKnownCurrencies []
