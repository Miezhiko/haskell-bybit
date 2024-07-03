module Utils where

import           Types

import qualified Data.Text           as T
import qualified Data.Time           as Tm
import           Data.Time.Clock     as Clc
import qualified Data.Time.LocalTime as Tml

pass ∷ Applicative f => f ()
pass = pure ()

extractValues ∷ OrderData -> Maybe (T.Text, T.Text)
extractValues (OrderData _ myData) =
  let bList = b myData
  in if null bList
     then Nothing
     else Just (s myData, head (head bList))

zerotime ∷ Tm.LocalTime
zerotime = Tml.utcToLocalTime Tml.utc (Clc.UTCTime (Tm.fromGregorian 1 1 1) 0)