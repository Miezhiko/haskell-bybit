[![Haskell CI](https://github.com/Miezhiko/bb/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/bb/actions/workflows/haskell.yml)

Brick TUI to BYBIT derivatives API on wuss (secure WebSocket)
-------------------------------------------------------------

 - put `bb.yml` file in your workdir
 - press ESC or `q` to exit
 - setup needed tickers to track on `ticker.json` [this docs](https://bybit-exchange.github.io/docs/v5/ws/connect)
 - (actually yml is not used for now, work in progress thing)

`bb.yml` file example:

```yml
cfgKey: yourBBpublikKey
cfgSecret: yourBBsecretKeyItsUsuallyLonger
```

`ticker.json` example included:

```json
{
  "req_id": "btcusdt",
  "op": "subscribe",
  "args": [
      "orderbook.1.BTCUSDT",
      "orderbook.1.ETHUSDT",
      "orderbook.1.SOLUSDT"
  ]
}
```

```haskell
tickerUSDTs <- extractTickerUSDTs
writeIORef coinRefs (M.fromList $ map (\x -> (x, (0.0, zerotime))) tickerUSDTs)

void ∘ forkIO ∘ forever $ do
  message <- receiveData connection
  let jsonData = decode message :: Maybe OrderData
  case jsonData of
    Just dat ->
      case (extractValues dat) of
        Just (tt, p) -> do
          let ss = T.unpack tt
          newTime   <- getTime
          mcoinRefs <- readIORef coinRefs
          case M.lookup ss mcoinRefs of
            Just (coinWas, lastDiffTime) -> do
              let tDiff    = Tm.diffLocalTime newTime lastDiffTime
                  tDiffSec = (round $ Tm.nominalDiffTimeToSeconds tDiff) :: Integer
                  coinNowS = T.unpack p
                  coinNow  = read coinNowS :: Float
              when (tDiffSec > 10) $
                writeIORef coinRefs $ M.insert ss (coinNow, newTime) mcoinRefs
```
