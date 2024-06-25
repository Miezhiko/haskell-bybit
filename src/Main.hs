{-# LANGUAGE
    KindSignatures
  , RankNTypes
  #-}

module Main where

import           Config                (getCfg)
import           Conky
import           Futures
import           Version

import           Data.Kind

import           System.Console.GetOpt
import           System.Environment    (getArgs)
import           System.Exit

main ∷ IO ()
main = do (actions, _, _) <- getOpt RequireOrder options <$> getArgs
          Options { optBB = run
                  } <- foldl (>>=) (pure defaultOptions) actions
          run

newtype Options
  = Options { optBB :: IO () }

defaultOptions ∷ Options
defaultOptions = Options {
    optBB = runFuturesExec
  }

options ∷ [OptDescr (Options -> IO Options)]
options = [
  Option "v" ["version"]    (NoArg showV)               "Display Version",
  Option []  ["help"]       (NoArg (showHelp options))  "Display Help",
  Option "f" ["futures"]    (NoArg getF)                "Display Derivatives",
  Option "c" ["conky"]      (NoArg getC)                "Conky Mode",
  Option "t" ["ticker"]     (ReqArg gett "String")      "Display Some Ticker History"
  ]

runFuturesExec ∷ IO ()
runFuturesExec = getCfg >>= go

runConkyExec ∷ IO ()
runConkyExec = getCfg >>= runEnvironment

runTickerExec ∷ String -> IO ()
runTickerExec _ = putStrLn "not implemented"

gett ∷ ∀ (μ :: Type -> Type). Monad μ => String -> Options -> μ Options
gett arg ο = pure ο { optBB = runTickerExec arg }

getF ∷ ∀ τ β. τ -> IO β
getF _ = runFuturesExec >> exitSuccess

getC ∷ ∀ τ β. τ -> IO β
getC _ = runConkyExec >> exitSuccess
