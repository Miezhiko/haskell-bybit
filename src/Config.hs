{-# LANGUAGE
    DeriveAnyClass
  , DeriveGeneric
  , DerivingStrategies
  #-}

module Config
  ( Conf (..)
  , getCfg
  ) where

import           Data.Aeson
import qualified Data.Yaml    as Yaml

import           GHC.Generics

data Conf
  = Conf
      { cfgKey    :: String
      , cfgSecret :: String
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getCfg ∷ IO Conf
getCfg = Yaml.decodeFileThrow "bb.yml"
