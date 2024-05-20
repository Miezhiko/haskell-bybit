{-# LANGUAGE
    QuasiQuotes
  #-}

module Futures
  ( go
  ) where

import           Prelude.Unicode

import           Config

import           Wuss

import           Control.Concurrent (forkIO)
import           Control.Monad      (forever, unless, void)
import           Data.Aeson
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO
import           Network.WebSockets (ClientApp, receiveData, sendClose, sendTextData)

import           NeatInterpolation  (text)

data JsonData
  = JsonData
      { topic :: T.Text
      , data' :: Data
      }
  deriving (Show)

data Data
  = Data
      { s :: T.Text
      , b :: [[T.Text]]
      }
  deriving (Show)

instance FromJSON JsonData where
  parseJSON (Object v) =
    JsonData <$> v.: "topic" <*> v.: "data"
  parseJSON _ = fail "Expected an object"

instance FromJSON Data where
  parseJSON (Object v) =
    Data <$> v.: "s" <*> v.: "b"
  parseJSON _ = fail "Expected an object"

extractValues ∷ JsonData -> Maybe (T.Text, T.Text)
extractValues (JsonData _ myData) =
  let bList = b myData
  in if null bList
     then Nothing
     else Just (s myData, head (head bList))

ws ∷ ClientApp ()
ws connection = do
  putStrLn "Connected!"

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

  let loop = do
          line <- getLine
          unless (null line) $ do
              sendTextData connection (T.pack line)
              loop
  loop

  sendClose connection (T.pack "connection closed")

go ∷ Conf -> IO ()
go _cfg = do
  runSecureClient "stream.bybit.com" 443 "/v5/public/linear" ws
  putStrLn $ "a"
