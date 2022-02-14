
{-
Homework of Week04. practice of traces and managing errors in contracts.
Lesson: https://www.youtube.com/watch?v=sxRLzR0jdiY&list=PLNEK_Ejlx3x230-g-U02issX5BiWAgmSi&index=5
-}

{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}

module Week04.Homework where

import Data.Aeson            (FromJSON, ToJSON)
import Data.Functor          (void)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Ledger
import Ledger.Ada            as Ada
import Ledger.Constraints    as Constraints
import Plutus.Contract       as Contract
import Plutus.Trace.Emulator as Emulator

data PayParams = PayParams
    { ppRecipient :: PaymentPubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

-- ################################################################################################
-- ######################################## Problem A #############################################

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

{-
Implement a trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with
Wallet 2 as recipient, but with amounts given by the two arguments. There should be a delay of one
slot after each endpoint call.

payTrace _ _ = --IMPLEMENT ME!

-}
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace p1 p2 = do
    h <- activateContractWallet (knownWallet 1) payContract'
    callEndpoint @"pay" h PayParams 
        { ppRecipient = (mockWalletPaymentPubKeyHash $ knownWallet 2)
        , ppLovelace = p1
        }
    void $ Emulator.waitNSlots 1
    callEndpoint @"pay" h PayParams 
        { ppRecipient = (mockWalletPaymentPubKeyHash $ knownWallet 2)
        , ppLovelace = p2
        }
    void $ Emulator.waitNSlots 1

payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 10_000_000 20_000_000

{-
payTest2 requires more Lovelace than there is in the wallet. When the first transaction is executed,
payContract will throw an exeption and crash. That means that the second transaction (that should
pass because the wallet has 100 Ada and the Tx it's only 20Ada) won't be executed either.
-}
payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000


-- ################################################################################################
-- ######################################## Problem B #############################################
{-
Modify payContract so it doesn't crash when payTest2 runs. (Remember to change payContract to
payContract' in payTrace when testing). That will allow for the second transaction of payTest2
to be processed even if the first one wasn't.
-}

-- IMP: Didn't manage to resolve the problem, this solution is from Lars.
payContract' :: Contract () PaySchema Text ()
payContract' = do 
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    -- IMP: handleError goes here because submitTx is the only function that can fail?
    handleError (\err -> Contract.logInfo $ "Caught error: " ++ unpack err) $ void $ submitTx tx
    payContract'



