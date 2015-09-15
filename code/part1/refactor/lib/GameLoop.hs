-- | The 'GameLoop' module manages the care and feeding of the game loop.
module GameLoop (
    gameLoop
  ) where

-- from 'base'
import           Control.Monad      (unless)

-- from 'sdl2'
import           SDL.Video.Renderer (Renderer)

-- internal
import           Event              (doEvents)
import           GameState          (GameState (..))
import           Render             (render)

-- | Run the game loop
gameLoop :: Renderer  -- ^ The SDL rendering context
         -> GameState -- ^ The state at the start of this iteration of the loop
         -> IO ()
gameLoop r s = do
  -- render the game
  render r
  -- update the GameState based on the events
  s' <- doEvents s
  -- check to see if the user has quit
  let q = hasQuit s'
  -- if not, keep going
  unless q (gameLoop r s')

