[![Haskell CI](https://github.com/Miezhiko/bb/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/bb/actions/workflows/haskell.yml)

Brick TUI to BYBIT derivatives API on wuss (secure WebSocket)
-------------------------------------------------------------

 - put `bb.yml` file in your workdir
 - press ESC or `q` to exit
 - setup needed tickers to track on `ticker.json` [this docs](https://bybit-exchange.github.io/docs/v5/ws/connect)
 - (actually yml is not used for now, work in progress thing)
 - using [hodl](https://github.com/Miezhiko/hodl/tree/mawa) for storing conky values

`bb.yml` file example:

```yml
cfgKey: yourBBpublikKey
cfgSecret: yourBBsecretKeyItsUsuallyLonger
cfgSignals:
  "BTCUSDT" : [ 50000, 100000 ]
cfgPipe: False
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

conky integration can be generated with `--conky-gen`

```bash
${color #C0C0C0} Ticker USDT${alignr}Graph 
${color}BTC ${alignr}${color}${execpi 1 hodl BTCUSDT | grep -oP '(?<=^)-?\d*\.?\d{2}' | awk '{print $1 ~ /^-/ ? "${color FF9999}" substr($0, 2) : "${color 99FF99}" $0}'} ${color}${execgraph "hodl BTCUSDT_GRAPH" 17,210 C0C0C0 33A3A3 -lt}
${color}ETH ${alignr}${color}${execpi 1 hodl ETHUSDT | grep -oP '(?<=^)-?\d*\.?\d{2}' | awk '{print $1 ~ /^-/ ? "${color FF9999}" substr($0, 2) : "${color 99FF99}" $0}'} ${color}${execgraph "hodl ETHUSDT_GRAPH" 17,210 C0C0C0 33A3A3 -lt}
${color}SOL ${alignr}${color}${execpi 1 hodl SOLUSDT | grep -oP '(?<=^)-?\d*\.?\d{4}' | awk '{print $1 ~ /^-/ ? "${color FF9999}" substr($0, 2) : "${color 99FF99}" $0}'} ${color}${execgraph "hodl SOLUSDT_GRAPH" 17,210 C0C0C0 33A3A3 -lt}
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
