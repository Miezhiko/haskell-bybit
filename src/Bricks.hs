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
  | EventUpdateBTC T.Text
  | EventUpdateETH T.Text
  | EventUpdateSOL T.Text

data Name = Derivatives
  deriving (Eq, Ord, Show)

-- | State of the brick app. Contains the controls and any other required state
data BrickState
  = BrickState
      { _stTime :: !Tm.LocalTime
      , _stBTC  :: !T.Text
      , _stETH  :: !T.Text
      , _stSOL  :: !T.Text
      }

makeLenses ''BrickState
