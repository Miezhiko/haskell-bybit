{-# LANGUAGE
    TemplateHaskell
  #-}

module ConkyGen
  ( generateConkyConfig
  ) where

import           Config
import           Ticker

import           Data.Foldable    (for_)

import           System.Directory (getCurrentDirectory)

generateConkyConfig ∷ Conf -> IO ()
generateConkyConfig _cfg = do
  tickerUSDTs <- extractTickerUSDTs
  currentDir  <- getCurrentDirectory
  for_ tickerUSDTs $ \t ->
    let tticker = take 3 t
        cdircon = currentDir ++ "/conky/"
    in putStrLn $ "${color}"
               ++ tticker ++ "${alignr}${color}${execpi 1 grep -oP '(?<=^)[+-]?\\d*\\.?\\d+' "
               ++ cdircon ++ t
               ++ " | awk '{print $1 < 0 ? \"${color FF9999}\" substr($0, 2) : \"${color 99FF99}\" substr($0, 2)}'}"
               ++ " ${color}${execgraph \"awk '{print $1}' "
               ++ cdircon ++ t ++ "_GRAPH\" 17,210 C0C0C0 C0C0C0 -lt}"
