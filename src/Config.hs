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
import qualified Data.Map     as M
import qualified Data.Yaml    as Yaml

import           GHC.Generics

data Conf
  = Conf
      { cfgKey     :: String
      , cfgSecret  :: String
      , cfgSignals :: M.Map String [Float]
      , cfgPipe    :: Bool
      }
  deriving (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

getCfg ∷ IO Conf
getCfg = Yaml.decodeFileThrow "bybit.yml"
