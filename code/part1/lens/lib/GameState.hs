{-# LANGUAGE TemplateHaskell #-}
-- | The 'GameState' module contains a data type to model the state of the
-- game along with the functions that manipulate it.
module GameState (
    GameState(..)
  , defaultGameState
  -- * Lenses for 'GameState'
  , backgroundColour
  , hasQuit
  ) where

-- from 'lens'
import           Control.Lens.TH (makeLenses)

-- internal
import           Colour          (Colour (..))

-- | The state of the game.
data GameState = GameState {
    -- | The background colour
    _backgroundColour :: Colour
    -- | Whether or not the user has quit
  , _hasQuit          :: Bool
  } deriving (Eq, Show)

makeLenses ''GameState

-- | A default 'GameState'.
defaultGameState :: GameState
defaultGameState = GameState Blue False
