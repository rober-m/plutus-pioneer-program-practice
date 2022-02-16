{-
A more realistic minting policy (modifying the Free contract to allow only the owner of a specified
public key to mint and burn — instead of everybody like in the Free contract).
Lesson: https://www.youtube.com/watch?v=4SROikF8JwE&list=PLNEK_Ejlx3x0G8V8CDBnRDZ86POVsrfzw&index=4
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Week05.Signed where

import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Void (Void)
import GHC.Generics (Generic)
import Ledger hiding (mint, singleton)
import Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Playground.Contract (ToSchema, ensureKnownCurrencies, printJson, printSchemas, stage)
import Playground.TH (mkKnownCurrencies, mkSchemaDefinitions)
import Playground.Types (KnownCurrency (..))
import Plutus.Contract as Contract
import Plutus.Trace.Emulator as Emulator
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Wallet.Emulator.Wallet
import Prelude (IO, Show (..), String)

{-
IMP: ################ IMPORTANT ################

When executing this contract, different wallets will get different CurrencySymbols, which
didn't happen in the "Free" contract. That's because in the "Free" contract, the policy
was a constant, and in this contract the policy is a function that takes a pulic key as
parameter — and that is different for different wallets.

That means that if different wallets mint with this contract, the TokenName will be the same
but the CurrencySymbol will always be different. Producing compleately different AssetClasses.

IMP: ############# END OF IMPORTANT ############
-}

{-
We pass a value of type PaymentPubKeyHash as a parameter
-}
{-# INLINEABLE mkPolicy #-}
mkPolicy :: PaymentPubKeyHash -> () -> ScriptContext -> Bool
{-
We now need both the PaymentPubKeyHash and the ScriptContext.

To check if the transaction is signed by a specific public key, we can use the txSignedBy function:

    txSignedBy :: TxInfo -> PubKeyHash -> Bool

We can get TxInfo from ScriptContext by using scirptContextTxInfo, and PaymentPubKeyHash it's just
a wraper around PubKeyHash. We can get the PubKeyHash from PaymentPubKeyHash by using
unPaymentPubKeyHash.
-}
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) $ unPaymentPubKeyHash pkh

--We modify the policy to use the new parameter
policy :: PaymentPubKeyHash -> Scripts.MintingPolicy
policy pkh =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` PlutusTx.liftCode pkh

--We modify currSymbol to use the new parameter
curSymbol :: PaymentPubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

-- ########################### END OF THE ON-CHAIN CODE (MINTING POLICY) ##########################
-- ################################################################################################
-- ################################# START OF THE OFF-CHAIN CODE ##################################

data MintParams = MintParams
  { mpTokenName :: !TokenName,
    mpAmount :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
  -- We obtain our PaymentPubKeyHash to pass it to our value and policy
  pkh <- Contract.ownPaymentPubKeyHash
  --We pass our PaymentPubKeyHash to currSymbol
  let val = Value.singleton (curSymbol pkh) (mpTokenName mp) (mpAmount mp)
      --We pass our PaymentPubKeyHash to policy
      lookups = Constraints.mintingPolicy $ policy pkh
      tx = Constraints.mustMintValue val
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

-- ################################################################################################
-- ############################## BOILERPLATE FOR THE PLAYGROUND ##################################

mkSchemaDefinitions ''FreeSchema

mkKnownCurrencies []

-- ################################################################################################
-- ###################################### EMULATOR TRACE ##########################################

test :: IO ()
test = runEmulatorTraceIO $ do
  let tn = "ABC"
  h1 <- activateContractWallet (knownWallet 1) endpoints
  h2 <- activateContractWallet (knownWallet 2) endpoints
  callEndpoint @"mint" h1 $
    MintParams
      { mpTokenName = tn,
        mpAmount = 555
      }
  callEndpoint @"mint" h2 $
    MintParams
      { mpTokenName = tn,
        mpAmount = 444
      }
  void $ Emulator.waitNSlots 1
  callEndpoint @"mint" h1 $
    MintParams
      { mpTokenName = tn,
        mpAmount = -222
      }
  void $ Emulator.waitNSlots 1
