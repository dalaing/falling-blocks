-- | The 'GameState' module contains a data type to model the state of the
-- game along with the functions that manipulate it.
module GameState (
    GameState(..)
  , defaultGameState
  ) where

-- | The state of the game.
data GameState = GameState {
    -- | Whether or not the user has quit
    hasQuit :: Bool
  } deriving (Eq, Show)

-- | A default 'GameState'.
defaultGameState :: GameState
defaultGameState = GameState False
