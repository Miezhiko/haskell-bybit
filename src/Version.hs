{-# LANGUAGE
    KindSignatures
  , RankNTypes
  #-}

module Version
  ( showHelp
  , showMyV
  , showV
  ) where

import           System.Console.GetOpt
import           System.Exit

import           Data.Version          (showVersion)
import qualified Paths_bybit           as My

showMyV      ∷ String
showMyV      = showVersion My.version

showV        ∷ ∀ τ β. τ -> IO β
showV _      = putStrLn ("bybit v" ++ showMyV) >> exitSuccess

showHelp     ∷ ∀ τ β α. [OptDescr α] -> τ -> IO β
showHelp o _ = putStrLn (usageInfo "Usage: bybit [optional things]" o)
                  >> exitSuccess
