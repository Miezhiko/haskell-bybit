module Utils where

import           Types

import qualified Data.Text           as T
import qualified Data.Time           as Tm
import           Data.Time.Clock     as Clc
import qualified Data.Time.LocalTime as Tml

import           System.Exit
import           System.Process

pass ∷ Applicative f => f ()
pass = pure ()

extractValues ∷ OrderData -> Maybe (T.Text, T.Text)
extractValues (OrderData _ myData) =
  let bList = b myData
  in case bList of
    []    -> Nothing
    (x:_) -> case x of
              []    -> Nothing
              (y:_) -> Just (s myData, y)

zerotime ∷ Tm.LocalTime
zerotime = Tml.utcToLocalTime Tml.utc (Clc.UTCTime (Tm.fromGregorian 1 1 1) 0)

checkExitCode ∷ ExitCode -> IO ()
checkExitCode ExitSuccess = pure ()
checkExitCode (ExitFailure γ) =
    error $ "failed with exit code: " ++ show γ

raw ∷ String -> [String] -> IO ()
raw λ α = rawSystem λ α >>= checkExitCode
