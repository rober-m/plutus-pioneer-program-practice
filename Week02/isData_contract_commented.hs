
{-
This contract is basically the same as the Typed contract but with a few changes.
I'll only comment the changes, everything else, look for in the gift, FortyTwo and 
Typed contracts.
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Week02.IsData where

import           Control.Monad        hiding (fmap)
import           Data.Map             as Map
import           Data.Text            (Text)
import           Data.Void            (Void)
import           Plutus.Contract
import           PlutusTx             (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Playground.Contract  (printJson, printSchemas, ensureKnownCurrencies, stage)
import           Playground.TH        (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types     (KnownCurrency (..))
import           Prelude              (IO, Semigroup (..), String)
import           Text.Printf          (printf)

-- We define a custom data type for our redeemer that just wraps an Integer
newtype MySillyRedeemer = MySillyRedeemer Integer

{-
This line uses template Haskell to provide (in compile time) an instance of IsData to
the type provided (MysillyRedeemer) and splice it into the source code at this point .

This is needed because we'll need to use the fromData and toData functions to convert our
custom type from/to BuiltinData (see the tx definition in the grab function).

Note: In production, you should use stableMakeIsData (instead of unstableMakeIsData) and 
specify which constructor you want to use. That's to make sure your code works in different 
versions of Plutus that may how the unstableMakeIsData function works.
-}
PlutusTx.unstableMakeIsData ''MySillyRedeemer

{-# INLINABLE mkValidator #-}
-- We have to specify our custom data type in the signature of mkValidator
mkValidator :: () -> MySillyRedeemer -> ScriptContext -> Bool
-- We pattern-match MySillyRedeemer to use r in our validation logic
mkValidator _ (MySillyRedeemer r) _ = traceIfFalse "wrong redeemer" $ r == 42

data Typed
instance Scripts.ValidatorTypes Typed where
    type instance DatumType Typed = ()
    -- We record that MySillyRedeemer is the type of our redeemer
    type instance RedeemerType Typed = MySillyRedeemer

typedValidator :: Scripts.TypedValidator Typed
typedValidator = Scripts.mkTypedValidator @Typed
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    -- We pass MySillyRedeemer to wrapValidator
    wrap = Scripts.wrapValidator @() @MySillyRedeemer

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

type GiftSchema =
            Endpoint "give" Integer
        .\/ Endpoint "grab" Integer

give :: AsContractError e => Integer -> Contract w s e ()
give amount = do
    let tx = mustPayToTheScript () $ Ada.lovelaceValueOf amount
    ledgerTx <- submitTxConstraints typedValidator tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "made a gift of %d lovelace" amount

grab :: forall w s e. AsContractError e => Integer -> Contract w s e ()
grab r = do
    utxos <- utxosAt scrAddress
    let orefs   = fst <$> Map.toList utxos
        lookups = Constraints.unspentOutputs utxos      <>
                  Constraints.otherScript validator
        tx :: TxConstraints Void Void
        -- We can pass (MySillyRedeemer r) to toBuiltinData because of what we deed before with unstableMakeIsData
        tx      = mconcat [mustSpendScriptOutput oref $ Redeemer $ PlutusTx.toBuiltinData (MySillyRedeemer r) | oref <- orefs]
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ "collected gifts"

endpoints :: Contract () GiftSchema Text ()
endpoints = awaitPromise (give' `select` grab') >> endpoints
  where
    give' = endpoint @"give" give
    grab' = endpoint @"grab" grab

mkSchemaDefinitions ''GiftSchema

mkKnownCurrencies []
