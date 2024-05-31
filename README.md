[![Haskell CI](https://github.com/Miezhiko/bb/actions/workflows/haskell.yml/badge.svg)](https://github.com/Miezhiko/bb/actions/workflows/haskell.yml)

Brick TUI to BYBIT derivatives API on wuss (secure WebSocket)
-------------------------------------------------------------

 - put `bb.yml` file in your workdir
 - press ESC or `q` to exit
 - (actually yml is not used for now, work in progress thing)

`bb.yml` file example:

```yml
cfgKey: yourBBpublikKey
cfgSecret: yourBBsecretKeyItsUsuallyLonger
```

```haskell
void ∘ forkIO ∘ forever $ do
  threadDelay 100000
  message <- receiveData connection
  let jsonData = decode message :: Maybe OrderData
  case jsonData of
    Just dat ->
      case (extractValues dat) of
        Just (ss, p) -> do
          t <- getTime
          BCh.writeBChan chan $ EventUpdateTime t
          case ss of
            "BTCUSDT" -> BCh.writeBChan chan $ EventUpdateBTC p
            "ETHUSDT" -> BCh.writeBChan chan $ EventUpdateETH p
            "SOLUSDT" -> BCh.writeBChan chan $ EventUpdateSOL p
            _         -> pass
        Nothing      -> pass
    Nothing -> pass
```
