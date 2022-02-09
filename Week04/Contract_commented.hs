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

myContract1 :: Contract () Empty Text ()
myContract1 = do
    void $ Contract.throwError "BOOM!"
    Contract.logInfo @String "hello from the contract"

myTrace1 :: EmulatorTrace ()
myTrace1 = void $ activateContractWallet (knownWallet 1) myContract1

test1 :: IO ()
test1 = runEmulatorTraceIO myTrace1

myContract2 :: Contract () Empty Void ()
myContract2 = Contract.handleError
    (\err -> Contract.logError $ "caught: " ++ unpack err)
    myContract1

myTrace2 :: EmulatorTrace ()
myTrace2 = void $ activateContractWallet (knownWallet 1) myContract2

test2 :: IO ()
test2 = runEmulatorTraceIO myTrace2

type MySchema = Endpoint "foo" Int .\/ Endpoint "bar" String

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
