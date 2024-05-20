module Types
  ( OrderData (..)
  , OrderDetails (..)
  ) where

import           Data.Aeson
import qualified Data.Text  as T

data OrderData
  = OrderData
      { topic :: T.Text
      , data' :: OrderDetails
      }
  deriving (Show)

data OrderDetails
  = OrderDetails
      { s :: T.Text
      , b :: [[T.Text]]
      }
  deriving (Show)

instance FromJSON OrderData where
  parseJSON (Object v) =
    OrderData <$> v.: "topic" <*> v.: "data"
  parseJSON _ = fail "Expected an object"

instance FromJSON OrderDetails where
  parseJSON (Object v) =
    OrderDetails <$> v.: "s" <*> v.: "b"
  parseJSON _ = fail "Expected an object"
