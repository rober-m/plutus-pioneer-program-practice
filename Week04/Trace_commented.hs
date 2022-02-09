
{-

This contract is used to explain what the EmulatorTrace monad is and how it works
Lesson: https://www.youtube.com/watch?v=qoUfgaHs1CI&list=PLNEK_Ejlx3x230-g-U02issX5BiWAgmSi&index=3

-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds        #-}

module Week04.Trace where

import Control.Monad.Freer.Extras as Extras
import Data.Default               (Default (..))
import Data.Functor               (void)
import Ledger.TimeSlot
import Plutus.Trace
import Wallet.Emulator.Wallet

-- I import the Vesting contract
import Week04.Vesting

-- Contract w s e a
-- EmulatorTrace a

{- 
The runEmulatorTraceIO function takes the EmulatorTrace that I'll define below,
runs it, and displays the result on the console.
 -}
test :: IO ()
test = runEmulatorTraceIO myTrace

{-
Here, I define a trace. The trace will execute some actions.
-}
myTrace :: EmulatorTrace ()
myTrace = do
    {-
    Before calling endpoints, we need to start the contract itself.
    That's what the activateContractWallet function does.
    activateContractWallet takes two arguments: A Wallet and a Contract, starts
    the contract as a side-effect, and returns a ContractHandle that we can use to
    reference the contract later.

    knownWallet takes an Integer and returns a Wallet. The emulator has 10 wallets
    pre-defined by default. By passing 1, we obtain the first Wallet of the emulator.
    -}
    h1 <- activateContractWallet (knownWallet 1) endpoints
    -- We do the same for the second wallet
    h2 <- activateContractWallet (knownWallet 2) endpoints
    {-
    We use the callEndpoint function to call specific endpoints specified in the Vesting
    contract. First we specify the endpoint we want to run, and then we specify
    which wallet runs it with the ContractHandle. If the endpoint needs some parameters 
    (like GiveParams in this case), we have to pass them to the ContractHandle.
    -}
    callEndpoint @"give" h1 $ GiveParams
        -- We extract the PubKeyHash from the second wallet and set it as the beneficiary
        { gpBeneficiary = mockWalletPaymentPubKeyHash $ knownWallet 2
        {-
        By passing the SlotConfig and a Slot to slotToBeginPOSIXTime, we get the starting 
        POSIXTime of the given Slot. We can pass 20 to set the deadline at the Slot 20.

        def uses the default SlotConfig to calculate the Slot. This works because
        we are using runEmulatorTraceIO without passing a special configuration, which means
        that it uses the default.
        -}
        , gpDeadline    = slotToBeginPOSIXTime def 20
        , gpAmount      = 10000000
        }
    {-
    Now, we wait until the 20th slot.
    
    waitUntilSlot takes a Slot, produces the side-effect of waiting until said Slot, and
    returns the Slot.

    We use the void function to ignore the result (void :: Functor f => f a -> f ()).
    -}
    void $ waitUntilSlot 20
    -- We call the grab endpoit whith wallet two. grab din't take any parameters, so we pass ().
    callEndpoint @"grab" h2 ()
    {-
    waitNSlots takes a number of type Natural (Type representing arbitrary-precision non-negative integers)
    and returns the Slot. This time we won't ignore it. We'll assign the result to 's' because we'll use it.
    -}
    s <- waitNSlots 2
    {-
    We log that we reached the slot 's'. This log is different because it's not from inside the contract
    but from the trace itself. That way, we know when we read the logs that the previous transactions 
    shou've ocurred by now.
    -}
    Extras.logInfo $ "reached " ++ show s
