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
  message <- receiveData connection
  let jsonData = decode message :: Maybe OrderData
  case jsonData of
    Just dat ->
      case (extractValues dat) of
        Just (ss, p) -> do
          newTime <- getTime
          BCh.writeBChan chan $ EventUpdateTime newTime
          let chainType = 
                case ss of
                  "BTCUSDT" -> Just (EventUpdateBTC, btcTRef, btcRef)
                  "ETHUSDT" -> Just (EventUpdateETH, ethTRef, ethRef)
                  "SOLUSDT" -> Just (EventUpdateSOL, solTRef, solRef)
                  _         -> Nothing
          case chainType of
            Just (cht, coinTRef, coinRef) -> do
              lastDiffTime <- readIORef coinTRef
              let tDiff    = Tm.diffLocalTime newTime lastDiffTime
                  tDiffSec = (round $ Tm.nominalDiffTimeToSeconds tDiff) :: Integer
                  coinNow  = (read $ T.unpack p) :: Float
              coinWas <- readIORef coinRef
              when (tDiffSec > 10) $ do
                writeIORef coinTRef newTime
                writeIORef coinRef coinNow
              BCh.writeBChan chan $ cht (coinNow > coinWas, p)
            Nothing  -> pass
        Nothing      -> pass
```
