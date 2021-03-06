{-

This contract is used to explain what the Contract monad is and how it works
Lesson: https://www.youtube.com/watch?v=yKX5Ce8Y0VQ&list=PLNEK_Ejlx3x230-g-U02issX5BiWAgmSi&index=4

-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Contract where

import Control.Monad.Freer.Extras as Extras
import Data.Functor               (void)
import Data.Text                  (Text, unpack)
import Data.Void                  (Void)
import Plutus.Contract            as Contract
import Plutus.Trace.Emulator      as Emulator
import Wallet.Emulator.Wallet

{- 
Overview of the Contract monad parameter types:

              Contract w s e a

- w -> Allow the contract to write messages of type 'w' to communicate with othe contracts. 'w' is visible from outside world.
       We can also use this feature to interact with the UI of our Dapp
    
- s -> Specifies the endpoints available in the contract.

- e -> Type of the error messages.

- a -> As with all monads, the last one is the result type. 'a' is the overall result of the computation.

-}

-- EmulatorTrace a

-- ################################################################################################
-- ######################################## EXAMPLE 1 #############################################

{-
We'll create a contract that doesn't write messages (unit), doesn't have endpoints (Empty), has
errors of type Text, and returns unit. The only thing that we contract will do is to log something.

We use Text because it's much more efficient than String for text data.
-}
myContract1 :: Contract () Empty Text ()
myContract1 = do
    {-
    thrwowError throws an exeption with any message that we want to specify (of type Text in this case).
    If the contract throws an exeption, it stops the execution right there. So, if we thrrow an exeption
    here, we wont see the log that we'll add below.
    
    We have to specify that thrwoError comes from Contract because the EmulatorTrace monad also has a
    throwError function.
    
    We don't need to specify @String like logInfo below because we already indicated the type in the
    signature of myContract1.

    We pass the result of throwError to void to a-void :D the warning that we are not using
    the value. 
    -}
    void $ Contract.throwError "BOOM!"
    {-
    logInfo (polimorphic) takes anything that's serializable (has ToJSON instance), logs it, and 
    returns a Contract.
    In this case we pass a literal string. But because we are using the OverloadedStrings extension,
    logInfo doesn't know which type it is. So we specify the type by adding @String using TypeApplications.

    We have to specify that logInfo comes from Contract because the EmulatorTrace monad also has a
    logInfo function.
    -}
    Contract.logInfo @String "hello from the contract"

-- I'll create an EmulatorTrace called myTrace1
myTrace1 :: EmulatorTrace ()
{-
The only thing myTrace1 will do is to activate myContract1. We'll pass Wallet 1 because we need
to pass a wallet. Because we spacified that we'll return unit, we have to change the ContractHandle
that activateContractWallet returns for a unit using void.

This will execute the contract and the contract will log the message we wrote.
-}
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1

-- To test myTrace1 we use runEmulatorTraceIO
test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

-- ################################################################################################
-- ######################################## EXAMPLE 2 #############################################
-- In this example, we'll run myContract1 of EXAMPLE 1, but catch and handle the exeption.

{-
Same signature as myContract1 but we changed the error type to Void.

Void (Data.Void) is a type that has no values. It's different than unit because unit has only one
value: the unit. But Void has no value at all.

By changing the error type to Void, we indicate that this contract can't possibly throw an exeption.
Because you vould have to provide a value of type Void to throwError, and there is no value of
type Void.
-}
myContract2 :: Contract () Empty Void ()
{-
To handle the exeption we'll use handleError:

handleError :: (e -> Contract w s e' a) -> Contract w s e a -> Contract w s e' a

handleError takes two arguments: A handle from "e" to a Contract where the error could potentiallly
change "e'", and a Contract with error of type "e". And returns the same Contract but with the error
type of "e'".

handleError executes Contract provided as a second argument. If there is no exeption, we get the result
of type "a" and handleError returns that "a" without modification. If there is an exeption of type "e",
then we apply our handler and get a contract with error type "e'" and run that one instead.
-}
myContract2 = Contract.handleError
    {-
    "err" is of type Text because myContract1 (second argument) has an error of type Text.
    logError is the same as logInfo but logs the message at the Error level.

    Because we want to concatenatenate "caught: " with the error and the error is of type Text,
    we have to convert it to type String. Text provides a function for just that called unpack.

    This time I don't need to specify @String because unpack returns a String and the compiler
    can infer that the result of the concatenation is a text of type String.
    -}
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

-- ################################################################################################
-- ######################################## EXAMPLE 3A #############################################

{-
We define a type synonim (the convention is that the name contains "Schema" in it).

Inside MySchema we define two endpoints: The foo and the bar endpoints.

We define an endpoint by passing t TODO: 

Example:
            Endpoint "foo" Int

-}
type MySchema = Endpoint "foo" Int

myContract3a :: Contract () MySchema Text ()
    {-
        TODO: Try to understand better the "endpoint" function -> https://youtu.be/yKX5Ce8Y0VQ?t=1102
    The signature of endpoint after the constraints is:
        
        endpoint :: (a -> Contract w s e b) -> Promise w s e b

    Promise is a Contract that is block awaiting to be triggered by an outside stimulus.

    @"foo" is a Type level string, and Contract.logInfo is a function of type Int -> Contract like
    we specified in MySchema.

    We need a Contract, not a Promise. So we use the function awaitPromise. awaitPromise will wait
    until the endpoint is called with an Int, and will return The Contract with that Int passed.
    -}
myContract3a = awaitPromise $ endpoint @"foo" Contract.logInfo


myTrace3a :: EmulatorTrace ()
myTrace3a = do
    {-
    This time -- unlike myTrace2 -- it's not enough to activate the contract, because the
    awaitPromise will keep awaiting forever until someone calls the endpoint.

    In order to call the endpoint, we need the handle. So, instead of throwing it away using 
    void, we'll bind it to a variable called "h" and reference it later.

    To call the endpoint we'll use the callEndpoint fuction which takes a ContractHandle and 
    the value that we want to provide to that endpint, and it invikes the endpoint.
    We also need to tell the compiler which endpoint we want to call using the @"type". I didn't
    understand yet how @"type" works in the endpoint function.
    -}
    h <- activateContractWallet (knownWallet 1) myContract3a
    callEndpoint @"foo" h 42

test3a :: IO ()
test3a = runEmulatorTraceIO myTrace3a

-- ################################################################################################
-- ######################################## EXAMPLE 3B #############################################

{-
The symbol ".\/" is a type operator of Plutus that concatenates Row elements.
A type operator is like a type constructor like Maybe, List, or Either.

To use type operators, we need to use the extension TypeOperators.
Endpoints are Row elements.

We create another Endpiont called bar that takes a String
-}
type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

myContract3b :: Contract () MySchema Text ()
{-
We need to add the "do" block because we're doing multiple operations.
-}
myContract3b = do
    awaitPromise $ endpoint @"foo" Contract.logInfo
    -- Same as previous line but with the bar endpoint.
    awaitPromise $ endpoint @"bar" Contract.logInfo

myTrace3b :: EmulatorTrace ()
myTrace3b = do
    h <- activateContractWallet (knownWallet 1) myContract3b
    callEndpoint @"foo" h 42
    -- Same as previous line but with the bar endpoint.
    callEndpoint @"bar" h "Haskell"

test3b :: IO ()
test3b = runEmulatorTraceIO myTrace3b


-- ################################################################################################
-- ######################################## EXAMPLE 4 #############################################

{-
The "w" in "Contratct w s e a" has to be an instance of the Monoid type class. Lists are an instance
of Monoids (they have mempty and mappend), so we'll use a list of Ints as "w".

We won't have endpoints, so we pass "Empty". We'll use Text as the type of our errors, and we'll
return unit.
-}

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    -- We wait for 10 Slots
    -- We use void because we don't need the value returned.
    void $ Contract.waitNSlots 10
    {-
    To use "w", we'll use the "tell" function that takes one argument (the state that you want to
    write, in this case a list with a single element) and mappends to the previous state.

    The state starts with mempty, adn each time "tell" is used, it mappends the current value to
    the prevous state.

    -}
    -- Here, the state is [].
    tell [1]
    -- Here, the state is [1].
    void $ Contract.waitNSlots 10
    tell [2]
    -- Here, the state is [1,2].
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4

    -- We wait for 5 Slots 
    void $ Emulator.waitNSlots 5
    {-
    using observableState we can look up the state of a running contract. observableState takes
    a handle and returns the state of the contract at that time. We bind the state to "xs".
    -}
    xs <- observableState h
    -- We log the state to inspect it. And check "xs" before tell [1].
    {- At this stage, the value of "xs" is the mempty value for the correspondent Monoid (an
    empty list of Ints: []).
    -}
    Extras.logInfo $ show xs

    {-
    We repeat the previous 3 operations two more times to check the state after tell [1] but 
    before tell [2], and after tell [2].
    -}
    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
