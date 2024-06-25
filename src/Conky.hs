{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  #-}

module Conky
  ( runEnvironment
  ) where

import           Prelude.Unicode

import           Config
import           Types

import           Wuss

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, void, when)

import           Data.Aeson          (decode)
import           Data.IORef
import qualified Data.Text           as T
import qualified Data.Time           as Tm
import           Data.Time.Clock     as Clc
import qualified Data.Time.LocalTime as Tml

import           System.IO.Unsafe

import           Network.WebSockets  (ClientApp, receiveData, sendClose, sendTextData)

import           NeatInterpolation   (text)

-- | Do nothing returning unit inside applicative.
pass ∷ Applicative f => f ()
pass = pure ()

extractValues ∷ OrderData -> Maybe (T.Text, T.Text)
extractValues (OrderData _ myData) =
  let bList = b myData
  in if null bList
     then Nothing
     else Just (s myData, head (head bList))

btcRef  ∷ IORef Float
ethRef  ∷ IORef Float
solRef  ∷ IORef Float

btcTRef ∷ IORef Tm.LocalTime
ethTRef ∷ IORef Tm.LocalTime
solTRef ∷ IORef Tm.LocalTime

zerotime ∷ Tm.LocalTime
zerotime = Tml.utcToLocalTime Tml.utc (Clc.UTCTime (Tm.fromGregorian 1 1 1) 0)

btcRef  = unsafePerformIO $ newIORef 0.0
ethRef  = unsafePerformIO $ newIORef 0.0
solRef  = unsafePerformIO $ newIORef 0.0

btcTRef = unsafePerformIO $ newIORef zerotime
ethTRef = unsafePerformIO $ newIORef zerotime
solTRef = unsafePerformIO $ newIORef zerotime
  
ws ∷ ClientApp ()
ws connection = do

  void ∘ forkIO ∘ forever $ do
    message <- receiveData connection
    let jsonData = decode message :: Maybe OrderData
    case jsonData of
      Just dat ->
        case (extractValues dat) of
          Just (ss, p) -> do
            newTime <- getTime
            let chainType = 
                  case ss of
                    "BTCUSDT" -> Just ("BTCUSDT", btcTRef, btcRef)
                    "ETHUSDT" -> Just ("ETHUSDT", ethTRef, ethRef)
                    "SOLUSDT" -> Just ("SOLUSDT", solTRef, solRef)
                    _         -> Nothing
            case chainType of
              Just (cht, coinTRef, coinRef) -> do
                lastDiffTime <- readIORef coinTRef
                let tDiff    = Tm.diffLocalTime newTime lastDiffTime
                    tDiffSec = (round $ Tm.nominalDiffTimeToSeconds tDiff) :: Integer
                    coinNowS = T.unpack p
                    coinNow  = read coinNowS :: Float
                coinWas <- readIORef coinRef
                when (tDiffSec > 10) $ do
                  writeIORef coinTRef newTime
                  writeIORef coinRef coinNow
                -- coinNow > coinWas (for color)
                writeFile cht coinNowS
              Nothing  -> pass
          Nothing      -> pass
      Nothing -> pass

  let subText = [text|
{
    "req_id": "btcusdt",
    "op": "subscribe",
    "args": [
        "orderbook.1.BTCUSDT",
        "orderbook.1.ETHUSDT",
        "orderbook.1.SOLUSDT"
    ]
}
  |]

  sendTextData connection subText

  qLoop

  sendClose connection (T.pack "connection closed")
 where
  -- | Get the local time
  getTime = do
    t   <- Tm.getCurrentTime
    tz  <- Tm.getCurrentTimeZone
    pure $ Tm.utcToLocalTime tz t
  
  qLoop = do
    c <- getChar
    if c == 'q' then return () else qLoop

runEnvironment ∷ Conf -> IO ()
runEnvironment _cfg = do
  runSecureClient "stream.bybit.com"
              443 "/v5/public/linear" ws
