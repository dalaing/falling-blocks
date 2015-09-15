-- | The 'GameState' module contains a data type to model the state of the
-- game along with the functions that manipulate it.
module GameState (
    GameState(..)
  , defaultGameState
  ) where

-- internal
import           Colour (Colour (..))

-- | The state of the game.
data GameState = GameState {
    backgroundColour :: Colour  -- ^ The background colour
  , hasQuit          :: Bool    -- ^ Whether or not the user has quit
  } deriving (Eq, Show)

-- | A default 'GameState'.
defaultGameState :: GameState
defaultGameState = GameState Blue False
