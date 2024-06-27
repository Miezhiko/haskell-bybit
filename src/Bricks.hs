{-# LANGUAGE
    TemplateHaskell
  #-}

module Bricks where

import           Control.Lens.TH (makeLenses)

import qualified Data.Map        as M
import qualified Data.Text       as T
import qualified Data.Time       as Tm

-- | Events that can be sent
data Event
  = EventUpdateTime Tm.LocalTime
  | EventUpdateCoin (String, (Bool, T.Text))

data Name = Derivatives
  deriving (Eq, Ord, Show)

-- | State of the brick app. Contains the controls and any other required state
data BrickState
  = BrickState
      { _stTime :: !Tm.LocalTime
      , _stCoin :: !(M.Map String (Bool, T.Text))
      }

makeLenses ''BrickState
