{-# LANGUAGE
    QuasiQuotes
  , TemplateHaskell
  #-}

module Conky
  ( runEnvironment
  ) where

import           Prelude.Unicode

import           Config
import           Ticker
import           Types
import           Utils

import           Wuss

import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless, void)

import           Data.Aeson          (decode)
import           Data.IORef
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Time           as Tm

import           System.FilePath
import           System.IO.Unsafe

import           Network.WebSockets  (ClientApp, receiveData, sendClose, sendTextData)

coinRefs ∷ IORef (M.Map String ((Float, Float, Float), Tm.LocalTime))
coinRefs = unsafePerformIO $ newIORef M.empty

ws ∷ ClientApp ()
ws connection = do

  tickerUSDTs <- extractTickerUSDTs
  writeIORef coinRefs
    (M.fromList $ map (\x ->
      (x, ((0.0, 666666666.0, 0.0), zerotime))) tickerUSDTs
    )

  void ∘ forkIO ∘ forever $ do
    message <- receiveData connection
    let jsonData = decode message :: Maybe OrderData
    case jsonData of
      Just dat ->
        case (extractValues dat) of
          Just (tt, p) -> unless (T.null p) $ do
            let ss = T.unpack tt
            newTime   <- getTime
            mcoinRefs <- readIORef coinRefs
            case M.lookup ss mcoinRefs of
              Just ((coinWas, cMin, cMax), lastDiffTime) -> do
                let tDiff    = Tm.diffLocalTime newTime lastDiffTime
                    tDiffSec = (round $ Tm.nominalDiffTimeToSeconds tDiff) :: Integer
                    coinNowS = T.unpack p
                    coinNow  = read coinNowS :: Float
                    newNow = if (tDiffSec > 10)
                                  then coinNow
                                  else coinWas
                    newmin = if coinNow < cMin
                                  then coinNow
                                  else cMin
                    newmax = if coinNow > cMax
                                  then coinNow
                                  else cMax
                writeIORef coinRefs $ M.insert ss ((newNow, newmin, newmax), newTime) mcoinRefs
                let sign = if coinNow > coinWas
                            then "+"
                            else "-"
                writeFile ("conky" </> ss) $ sign ++ coinNowS
                let cGraph =
                      if newmax > newmin
                        then (coinNow - newmin) / (newmax - newmin) * 100
                        else 50.0
                writeFile ("conky" </> (ss ++ "_GRAPH")) $ show cGraph
              Nothing  -> pass
          Nothing      -> pass
      Nothing -> pass

  ts <- getTickerAsString
  sendTextData connection $ T.pack ts

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
