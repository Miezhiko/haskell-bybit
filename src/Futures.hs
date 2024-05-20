{-# LANGUAGE
    QuasiQuotes
  #-}

module Futures
  ( go
  ) where

import           Prelude.Unicode

import           Config
import           Types

import           Wuss

import           Control.Concurrent (forkIO)
import           Control.Monad      (forever, unless, void)
import           Data.Aeson         (decode)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)

import           NeatInterpolation  (text)

extractValues ∷ OrderData -> Maybe (T.Text, T.Text)
extractValues (OrderData _ myData) =
  let bList = b myData
  in if null bList
     then Nothing
     else Just (s myData, head (head bList))

ws ∷ ClientApp ()
ws connection = do
  putStrLn "Connected!"

  void ∘ forkIO ∘ forever $ do
    message <- receiveData connection
    let jsonData = decode message :: Maybe OrderData
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

  let loop = do
          line <- getLine
          unless (null line) $ do
              sendTextData connection (T.pack line)
              loop
  loop

  sendClose connection (T.pack "connection closed")

go ∷ Conf -> IO ()
go _cfg = do
  runSecureClient "stream.bybit.com"
                  443 "/v5/public/linear" ws
