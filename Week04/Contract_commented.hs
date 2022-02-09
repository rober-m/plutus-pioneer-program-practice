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

myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String


-- ################################################################################################
-- ######################################## EXAMPLE 3 #############################################

myContract3 :: Contract () MySchema Text ()
myContract3 = do
    awaitPromise $ endpoint @"foo" Contract.logInfo
    awaitPromise $ endpoint @"bar" Contract.logInfo

myTrace3 :: EmulatorTrace ()
myTrace3 = do
    h <- activateContractWallet (knownWallet 1) myContract3
    callEndpoint @"foo" h 42
    callEndpoint @"bar" h "Haskell"

test3 :: IO ()
test3 = runEmulatorTraceIO myTrace3


-- ################################################################################################
-- ######################################## EXAMPLE 4 #############################################

myContract4 :: Contract [Int] Empty Text ()
myContract4 = do
    void $ Contract.waitNSlots 10
    tell [1]
    void $ Contract.waitNSlots 10
    tell [2]
    void $ Contract.waitNSlots 10

myTrace4 :: EmulatorTrace ()
myTrace4 = do
    h <- activateContractWallet (knownWallet 1) myContract4

    void $ Emulator.waitNSlots 5
    xs <- observableState h
    Extras.logInfo $ show xs

    void $ Emulator.waitNSlots 10
    ys <- observableState h
    Extras.logInfo $ show ys

    void $ Emulator.waitNSlots 10
    zs <- observableState h
    Extras.logInfo $ show zs

test4 :: IO ()
test4 = runEmulatorTraceIO myTrace4
