{-# LANGUAGE FlexibleContexts      #-}
-- | The 'GameLoop' module manages the care and feeding of the game loop.
module GameLoop (
    gameLoop
  , startGameLoop
  ) where

-- from 'base'
import           Control.Monad        (unless)

-- from 'mtl'
import           Control.Monad.Reader (MonadReader, runReaderT)
import           Control.Monad.State  (MonadState, evalStateT)
import           Control.Monad.Trans  (MonadIO)

-- from 'lens'
import           Control.Lens.Getter  (use)

-- from 'sdl2'
import           SDL.Video.Renderer   (Renderer)

-- internal
import           Event                (doEvents)
import           GameState            (GameState (..), hasQuit)
import           Render               (render)

-- | Runs the game loop.
gameLoop :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
         => m ()
gameLoop = do
  -- update the GameState based on the events
  doEvents
  -- render the game
  render
  -- check to see if the user has quit
  q <- use hasQuit
  -- if not, keep going
  unless q gameLoop

-- | Starts the game loop.
startGameLoop :: Renderer -> GameState -> IO ()
startGameLoop r = evalStateT (runReaderT gameLoop r)
