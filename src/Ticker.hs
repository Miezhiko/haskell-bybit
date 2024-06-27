module Ticker
  ( Request (..)
  , extractTickerUSDTs
  , getTickerAsString
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as LB

data Request
  = Request
      { reqId :: String
      , op    :: String
      , args  :: [String]
      }
  deriving (Show)

instance FromJSON Request where
  parseJSON (Object v) = Request
    <$> v .: "req_id"
    <*> v .: "op"
    <*> v .: "args"
  parseJSON v = fail $ "Expected an object, but got " ++ show v

extractSymbols ∷ Request -> [String]
extractSymbols req = map (drop 9) (args req)

getTickerAsString ∷ IO String
getTickerAsString = readFile "ticker.json"

extractTickerUSDTs ∷ IO [String]
extractTickerUSDTs = do
  json <- LB.readFile "ticker.json"
  let request = decode json :: Maybe Request
  case request of
    Just req -> pure $ extractSymbols req
    Nothing  -> fail $ "failed to decode ticker.json"
