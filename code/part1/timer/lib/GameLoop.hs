{-# LANGUAGE FlexibleContexts      #-}
-- | The 'GameLoop' module manages the care and feeding of the game loop.
module GameLoop (
    gameLoop
  , startGameLoop
  ) where

-- from 'base'
import           Control.Monad        (unless, when)

-- from 'mtl'
import           Control.Monad.Reader (MonadReader, runReaderT)
import           Control.Monad.State  (MonadState, evalStateT)
import           Control.Monad.Trans  (MonadIO, liftIO)

-- from 'lens'
import           Control.Lens.Getter  (use, view)

-- internal
import           Config               (Config, HasConfig(..))
import           Clock                (waitForClock, watch)
import           Event                (doEvents)
import           GameState            (GameState (..), hasQuit)
import           Render               (render)

-- | Runs the game loop.
gameLoop :: (HasConfig c, MonadReader c m, MonadState GameState m, MonadIO m)
         => m ()
gameLoop = do
  -- render the game
  render
  -- wait for the clock to tick over
  c <- view uiClock
  waitForClock c
  l <- view logicClock
  check <- watch l
  when check . liftIO $
    putStrLn "tick"
  -- update the GameState based on the events
  doEvents
  -- check to see if the user has quit
  q <- use hasQuit
  -- if not, keep going
  unless q gameLoop

-- | Starts the game loop.
startGameLoop :: Config -> GameState -> IO ()
startGameLoop c = evalStateT (runReaderT gameLoop c)
