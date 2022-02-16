{-
Contract to mint/burn NFTs
Lesson: https://www.youtube.com/watch?v=2lKN0ZL_EQU&list=PLNEK_Ejlx3x0G8V8CDBnRDZ86POVsrfzw&index=5
-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Week05.NFT where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (FromJSON, ToJSON)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract        as Contract
import           Plutus.Trace.Emulator  as Emulator
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)
import           Wallet.Emulator.Wallet

{-
IMP: Explanation of the idea behind the code:

To have true NFTs, we must prevent to be more than one mintin transaction for a token. The minting
policy has to return True in only one transaction. It must be impossible to to the same again in
another transaction (even in the same Slot).

The key is that we need something in the Cardano blockchain that we can refer in our minting policy
that is unique and can only exist in one transaction and never again. That something is UTXOs

UTXOs can only exist once and all UTXOS are unique. To pinpoint a specifi UTXO, you have to pinpoint
a specific transaction via it's transaction ID, and in that transaction you have to specify which 
output of that transaciton is the UTXO needed via the index (UTXOS are ordered). 

So, UTXOs are unique because transactions are unique. And transactions are unique because of the
fees.
If it wasn't for the fees, we could have a transaction that has zero inputs and only outpts without
value. And such a transaction could exist multiple times, because we could have the exact same
inputs and exact same hash. But, in a system with fees (like Cardano), you can't have such
transaction becaus you'll always need to have some input to pay the fees. And as soon as you have
an input, that input has to come from somewhere — a previous UTXO. Because we can't have
double spending, no transaction will have the same inputs, and therefore, no trasnaction will have
the same hash (ID).

By passing a specific UTXO (it doesn't matter which one) as a parameter to our minting policy and
checking in the policy that the minting transaction consumes said UTXO, we can guarantee uniqueness.
-}

{-
We pass two parameters:
    - One of type TxOutRef that references the transaction output that we want to consume.
    - One of type TokenName with the NFT name.
-}
{-# INLINABLE mkPolicy #-}
mkPolicy :: TxOutRef -> TokenName -> () -> ScriptContext -> Bool
{-
We have to check two thigs: That the UTxO specified by oref is consumend, and that we only mint
the token once.
-}
mkPolicy oref tn () ctx = traceIfFalse "UTxO not consumed"   hasUTxO           &&
                          traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    {-
    Each transaction inputs is another transaction output.

    `any` is a standard Haskell function that checks if any (at least one) element of a list 
    satisfies a condition. `any` takes two parameters: A condition and a list.
    
    To get the inputs of our transaction we can pass our `info :: TxInfo` to
    `txInfoInputs` and we'll obtain a value of type [TxInInfo].

    Then, for each value (input) of that list, we extract the reference of the output that this
    value reference using `txInInfoOutRef ` and we check if it equals the given oref.

    If any of the outputs equals oref, we consumed said UTxO and we return True.
    -}
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    {-
    To extract the value minted by this transaction we use `txInfoMint` and pass the 
    transaction info.
    
    Once we obtained the value, we flatten the Map into a list of triples, and if we obtain a 
    list that doesn't contain exactly ONE triple with the token name that we specified as parameter
    and an ammount of one, we return False.
    -}
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn', amt)] -> tn' == tn && amt == 1
        _               -> False

-- We compile the policy with the two parameters
policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy
policy oref tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \oref' tn' -> Scripts.wrapMintingPolicy $ mkPolicy oref' tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode oref
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

-- We generate the CurrencySymbol with the two parameters
curSymbol :: TxOutRef -> TokenName -> CurrencySymbol
curSymbol oref tn = scriptCurrencySymbol $ policy oref tn

-- ########################### END OF THE ON-CHAIN CODE (MINTING POLICY) ##########################
-- ################################################################################################
-- ################################# START OF THE OFF-CHAIN CODE ##################################

{-
We could specify the UTxO as a parameter, but that would be inconvenient for the user because the
user would have to look up the info. Instead, we can pass the wallet address and use the Contract
monad to find a UTxO that we can use.

So we'll define the TokenName and wallet Address as the parameters.
-}
data NFTParams = NFTParams
    { npToken   :: !TokenName
    , npAddress :: !Address
    } deriving (Generic, FromJSON, ToJSON, Show)

type NFTSchema = Endpoint "mint" NFTParams

mint :: NFTParams -> Contract w NFTSchema Text ()
mint np = do
    {- We get all the UTxOs that are siting in the wallet address (which is a map from TxOutRef
    to the actual outputs).
    -}
    utxos <- utxosAt $ npAddress np
    -- We extract the keys of the utxos Map
    case Map.keys utxos of
        -- If there's no TxOutRef, we log an error and finish executing
        []       -> Contract.logError @String "no utxo found"
        {-
        If there's at least one TxOutRef, we just take the first one (using patter matching)
        because it doesn't really matter which one we use.
        -}
        oref : _ -> do
            let tn      = npToken np
            {- We create a Value using `singleton` and passing the CurrencySymbol, the TokenName,
            and a 1 (one) as the ammount.
            -}
            let val     = Value.singleton (curSymbol oref tn) tn 1
                {-
                As constraints we have to pass the minting policy and the utxos. We don't actually
                need all utxos, we only need the one we used to construct the Value (named "oref"
                above), but it doesn't matter if what we specify here is too big. utxos has to
                contain oref. So, to make our lifes easier, we pass al utxos sitting at that
                address.
                -}
                lookups = Constraints.mintingPolicy (policy oref tn) <> Constraints.unspentOutputs utxos
                {- Here we also need to specify, not only mustMintValue to make sure a value is
                minted/burned, but also mustSpendPubKeyOutput to make sure that the UTxO that we
                use to mint the value is spent and non one can repeat the transaction again.
                -}
                tx      = Constraints.mustMintValue val <> Constraints.mustSpendPubKeyOutput oref
            ledgerTx <- submitTxConstraintsWith @Void lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String $ printf "forged %s" (show val)

endpoints :: Contract () NFTSchema Text ()
endpoints = mint' >> endpoints
  where
    mint' = awaitPromise $ endpoint @"mint" mint

-- ################################################################################################
-- ###################################### EMULATOR TRACE ##########################################

test :: IO ()
test = runEmulatorTraceIO $ do
    let tn = "ABC"
        w1 = knownWallet 1
        w2 = knownWallet 2
    h1 <- activateContractWallet w1 endpoints
    h2 <- activateContractWallet w2 endpoints
    callEndpoint @"mint" h1 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w1
        }
    callEndpoint @"mint" h2 $ NFTParams
        { npToken   = tn
        , npAddress = mockWalletAddress w2
        }
    void $ Emulator.waitNSlots 1
