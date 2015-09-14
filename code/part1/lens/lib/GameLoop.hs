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

-- internal
import           Config               (Config, HasConfig)
import           Event                (doEvents)
import           GameState            (GameState (..), hasQuit)
import           Render               (render)

-- | Runs the game loop.
gameLoop :: (HasConfig c, MonadReader c m, MonadState GameState m, MonadIO m)
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
startGameLoop :: Config -> GameState -> IO ()
startGameLoop c = evalStateT (runReaderT gameLoop c)
