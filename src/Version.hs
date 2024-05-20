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
import qualified Paths_bb              as My

showMyV      ∷ String
showMyV      = showVersion My.version

showV        ∷ ∀ τ β. τ -> IO β
showV _      = putStrLn ("bb v" ++ showMyV) >> exitSuccess

showHelp     ∷ ∀ τ β α. [OptDescr α] -> τ -> IO β
showHelp o _ = putStrLn (usageInfo "Usage: bb [optional things]" o)
                  >> exitSuccess
