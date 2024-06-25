{-# LANGUAGE
    TemplateHaskell
  #-}

module Bricks where

import           Control.Lens.TH (makeLenses)

import qualified Data.Text       as T
import qualified Data.Time       as Tm

-- | Events that can be sent
data Event
  = EventUpdateTime Tm.LocalTime
  | EventUpdateBTC (Bool, T.Text)
  | EventUpdateETH (Bool, T.Text)
  | EventUpdateSOL (Bool, T.Text)

data Name = Derivatives
  deriving (Eq, Ord, Show)

-- | State of the brick app. Contains the controls and any other required state
data BrickState
  = BrickState
      { _stTime :: !Tm.LocalTime
      , _stBTC  :: !(Bool, T.Text)
      , _stETH  :: !(Bool, T.Text)
      , _stSOL  :: !(Bool, T.Text)
      }

makeLenses ''BrickState
