{-# LANGUAGE
    TemplateHaskell
  #-}

module ConkyGen
  ( generateConkyConfig
  ) where

import           Config
import           Ticker

import           Data.Foldable    (for_)

generateConkyConfig ∷ Conf -> IO ()
generateConkyConfig _cfg = do
  tickerUSDTs <- extractTickerUSDTs
  for_ tickerUSDTs $ \t ->
    let tticker = take 3 t
    in putStrLn $ "${color}"
               ++ tticker ++ "${alignr}${color}${execpi 1 hodl " ++ t
               ++ "| grep -oP '(?<=^)-?\\d*\\.?\\d{4}' | awk '{print $1 ~ /^-/ ? \"${color FF9999}\" substr($0, 2) : \"${color 99FF99}\" $0}'}"
               ++ " ${color}${execgraph \"hodl " ++ t ++ "_GRAPH\" 17,210 000000 33A3A3 -lt}"
