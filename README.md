[![Haskell CI](https://github.com/Miezhiko/bb/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/bb/actions/workflows/haskell.yml)

```haskell
  void ∘ forkIO ∘ forever $ do
    message <- receiveData connection
    let jsonData = decode message :: Maybe JsonData
    case jsonData of
      Just dat ->
        case (extractValues dat) of
          Just (ss, p) ->
            TIO.putStrLn $ T.concat [ss, ": ", p]
          Nothing      -> pure ()
      Nothing -> pure ()

  let subText = [text|
{
    "req_id": "btcusdt",
    "op": "subscribe",
    "args": [
        "orderbook.1.BTCUSDT"
    ]
}
  |]

  sendTextData connection subText
```
