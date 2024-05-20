{-# LANGUAGE
    KindSignatures
  , RankNTypes
  #-}

module Main where

import           Config                (Conf (cfgKey), getCfg)
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
    optBB = runPortfolioExec
  }

options ∷ [OptDescr (Options -> IO Options)]
options = [
  Option "v" ["version"]    (NoArg showV)               "Display Version",
  Option []  ["help"]       (NoArg (showHelp options))  "Display Help",
  Option "p" ["portfolio"]  (NoArg getP)                "Display Portfolio",
  Option "h" ["historical"] (NoArg getH)                "Display Historical Data",
  Option "t" ["ticker"]     (ReqArg gett "String")      "Display Some Ticker History"
  ]

runPortfolioExec ∷ IO ()
runPortfolioExec = do
  cfg <- getCfg
  putStrLn $ cfgKey cfg
  go cfg

runHistoricalExec ∷ IO ()
runHistoricalExec = putStrLn "aaa"

runTickerExec ∷ String -> IO ()
runTickerExec _ = putStrLn "aaa"

gett ∷ ∀ (μ :: Type -> Type). Monad μ => String -> Options -> μ Options
gett arg ο = pure ο { optBB = runTickerExec arg }

getP ∷ ∀ τ β. τ -> IO β
getP _ = runPortfolioExec >> exitSuccess

getH ∷ ∀ τ β. τ -> IO β
getH _ = runHistoricalExec >> exitSuccess
