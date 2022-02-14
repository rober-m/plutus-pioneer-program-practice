
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

payContractA :: Contract () PaySchema Text ()
payContractA = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContractA

{-
Implement a trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with
Wallet 2 as recipient, but with amounts given by the two arguments. There should be a delay of one
slot after each endpoint call.

payTrace _ _ = --IMPLEMENT ME!

-}
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace p1 p2 = do
    h <- activateContractWallet (knownWallet 1) payContract
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

payTest2 :: IO ()
payTest2 = runEmulatorTraceIO $ payTrace 1000_000_000 20_000_000


-- ################################################################################################
-- ######################################## Problem B #############################################
{-
Modify payContractA so it doesn't crash when payTest2 runs. (Remember to change payContractA to
payContractB in payTrace when testing)
-}

payContractB :: Contract () PaySchema Text ()
payContractB = do
    pp <- awaitPromise $ endpoint @"pay" return
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContractB
