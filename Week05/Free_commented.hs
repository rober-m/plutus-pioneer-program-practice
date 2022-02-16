{-
Simple minting policy.
Lesson: https://www.youtube.com/watch?v=DBUdFsZpW7A&list=PLNEK_Ejlx3x0G8V8CDBnRDZ86POVsrfzw&index=3
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

module Week05.Free where

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
This is a Minting Policy script (ScriptPurpose is Minting).
Validation scripts have three imputs (Datum, Redeemer, and ScriptContext), but Minting scripts have
only two (Redeemer and ScriptContext). Minting policy scripts don't have Datum because Datums sit
at the output of UTXOs that are being spent, and in this case ... //TODO

If there are more than two inputs, it's because it's a parameteryzed policy.

Notes
    - You can pass many CurrencySymbols at the same time and the corrsponding minting policy will
    be executed.
    - If only one of them fails, the entire transaction fails.
    - This is the typed version, but you can also use BuiltinData like on validators.
-}
{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
{-
By ignoring the Redeemer and ScriptContext, and returning always True, we are allowing the minting
and buring for the CurrencySimbol (the hash of the script) given by this policy.
-}
mkPolicy () _ = True

{-
Here, we compile the policy to Pulutus Script.
Instead of bein of type Scripts.typedValidator, it's Script.MintingPolicy (TODO: why not
typedMintingPolicy?).
-}
policy :: Scripts.MintingPolicy
{-
First, we pass our typed mkPolicy to Scripts.wrapMintingPolicy to convert it to the untyped version
that just uses BuiltinData for the arguments and a unit return type. For this to work, we need to
add {-# INLINEABLE mkPolicy #-} pragma.

Then, we compile to plutus core and use mkMintingPolicyScript to generate th MintingPolicy:

mkMintingPolicyScript :: CompiledCode (BuiltinData -> BuiltinData -> ()) -> MintingPolicy
-}
policy = mkMintingPolicyScript $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkPolicy||])

{-
We obtain the CurrencySymbol (hash of the script) by directly applying scriptCurrencySymbol to
the policy.
-}
curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- ########################### END OF THE ON-CHAIN CODE (MINTING POLICY) ##########################
-- ################################################################################################
-- ################################# START OF THE OFF-CHAIN CODE ##################################

{-
We pass the token name and the ammount that we want to mint or burn as params.
If the ammount is positive, we mint. If it's negative, we burn.
We only have to specify teh TokenName because the CurrencySymbol is given by this script.
-}
data MintParams = MintParams
  { mpTokenName :: !TokenName,
    mpAmount :: !Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

-- "FreeSchema" because the module is called "Free"
type FreeSchema = Endpoint "mint" MintParams

mint :: MintParams -> Contract w FreeSchema Text ()
mint mp = do
  {-
  Here, we compute the value that we want to forge.
  To compute it, we use the Value.singleton function:

  singleton :: CurrencySymbol -> TokenName -> Integer -> Value
  -}
  let val = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
      {-
      As lookups, we have to specify the mintingPolicy. So, the transaction has to contain
      the actual minting policy (script) for the validation to be successful.
      -}
      lookups = Constraints.mintingPolicy policy
      {-
      The only constraint that we put on our transaction is that it must mint the value
      computed. We do that by passing the value to mint to mustMintValue:

            mustMintValue :: forall i o. Value -> TxConstraints i o
      -}
      tx = Constraints.mustMintValue val
  {-
  By executing submitTxConstraintsWith, the algorithm will automatically take care of finding
  inputs to cover the fees, and it will automatically transfer the minted value to the wallet
  (if it's positive) or try to find suficiently many tokes in the wallet that can be burn and
  burn them.
  -}
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
  Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () FreeSchema Text ()
-- We recursively call teh endpoints
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
  -- Wallet 1 mints 555 tokens
  callEndpoint @"mint" h1 $
    MintParams
      { mpTokenName = tn,
        mpAmount = 555
      }
  -- Wallet 2 mints 444 tokens
  callEndpoint @"mint" h2 $
    MintParams
      { mpTokenName = tn,
        mpAmount = 444
      }
  -- Wallet 1 burns 222 tokens
  void $ Emulator.waitNSlots 1
  callEndpoint @"mint" h1 $
    MintParams
      { mpTokenName = tn,
        mpAmount = -222
      }
  void $ Emulator.waitNSlots 1
