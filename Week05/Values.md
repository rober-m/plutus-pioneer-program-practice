
# Values lecture
Lecture link: https://www.youtube.com/watch?v=4iNTgjovMRg&list=PLNEK_Ejlx3x0G8V8CDBnRDZ86POVsrfzw&index=2






### Transcript of the repl
Ok, five modules loaded.
Prelude Week05.Free> import Plutus.V1.Ledger.Value
Prelude Plutus.V1.Ledger.Value Week05.Free> import Plutus.V1.Ledger.Ada
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :set -XOverloadedStrings
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t adaSymbol
adaSymbol :: CurrencySymbol
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> adaSymbol

Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t adaToken
adaToken :: TokenName
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t lovelaceValueOf
lovelaceValueOf :: Integer -> Value
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> lovelaceValueOf 123
Value (Map [(,Map [("",123)])])
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> lovelaceValueOf 123 <> lovelaceValueOf 10
Value (Map [(,Map [("",133)])])
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t singleton
singleton :: CurrencySymbol -> TokenName -> Integer -> Value
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> singleton "a8ff" "ABC" 7
Value (Map [(a8ff,Map [("ABC",7)])])
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
Value (Map [(,Map [("",42)]),(a8ff,Map [("ABC",7),("XYZ",100)])])
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> let v = singleton "a8ff" "ABC" 7 <> lovelaceValueOf 42 <> singleton "a8ff" "XYZ" 100
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> v
Value (Map [(,Map [("",42)]),(a8ff,Map [("ABC",7),("XYZ",100)])])
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t valueOf
valueOf :: Value -> CurrencySymbol -> TokenName -> Integer
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> valueOf v "a8ff" "XYZ"
100
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> valueOf v "a8ff" "ABC"
7
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> valueOf v "a8ff" "abc"
0
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> :t flattenValue
flattenValue :: Value -> [(CurrencySymbol, TokenName, Integer)]
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free> flattenValue v
[(,"",42),(a8ff,"XYZ",100),(a8ff,"ABC",7)]
Prelude Plutus.V1.Ledger.Value Plutus.V1.Ledger.Ada Week05.Free>