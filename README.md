[![Haskell CI](https://github.com/Miezhiko/Rekishi/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/Rekishi/actions/workflows/haskell.yml)

```haskell
getCandlesWith ∷ Timestamp -- from
              -> Timestamp -- to
              -> GrpcClient
              -> T.Text 
              -> IO [HistoricCandle]
getCandlesWith tfrom tto g myFigi = do
  let Just daily  = maybeToEnum 5
  let gcr = build $ ( MD.figi     .~ myFigi )
                  ∘ ( MD.from     .~ tfrom )
                  ∘ ( MD.to       .~ tto )
                  ∘ ( MD.interval .~ daily )
  runGetCandles g gcr
```
