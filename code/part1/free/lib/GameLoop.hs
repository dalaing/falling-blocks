{-# LANGUAGE FlexibleContexts      #-}
-- | The 'GameLoop' module manages the care and feeding of the game loop.
module GameLoop (
    gameLoop
  , startGameLoop
  ) where

-- from 'base'
import           Control.Monad        (when, forever)

-- from 'mtl'
import           Control.Monad.Reader (MonadReader, runReaderT)
import           Control.Monad.State  (MonadState, evalStateT)
import           Control.Monad.Trans  (MonadIO, liftIO)

-- from 'free'
import           Control.Monad.Trans.Free   (MonadFree, FreeT(..), FreeF(..))

-- from 'lens'
import           Control.Lens.Getter  (view)
import           Control.Lens.Setter  ((%=))

-- internal
import           Config               (Config, HasConfig(..))
import           Clock                (waitForClock, watch)
import           Event                (AppEvent(..), renderEvent, timerEvent, doEvents)
import           Colour               (nextColour)
import           GameState            (GameState (..), backgroundColour)
import           Render               (render)

gameStep :: (HasConfig c, MonadReader c m, MonadFree AppEvent m, MonadIO m)
         => m ()
gameStep = do
  -- start with a render event
  renderEvent

  -- wait for the ui clock to tick over
  c <- view uiClock
  waitForClock c

  -- grab the SDL events that have happened while we were waiting
  doEvents

  -- check to see if we've had a logic clock event
  l <- view logicClock
  check <- watch l
  when check timerEvent

interpret :: (HasConfig c, MonadReader c m, MonadState GameState m, MonadIO m)
         => FreeT AppEvent m ()
         -> m ()
interpret f = do
  x <- runFreeT f
  case x of
    Pure y -> return y
    Free z -> case z of
      AppRender k -> do
        render
        interpret k
      AppCycle k -> do
        backgroundColour %= nextColour
        interpret k
      AppTimer k -> do
        liftIO $ putStrLn "tick"
        interpret k
      AppQuit -> return ()

-- | Runs the game loop.
gameLoop :: (HasConfig c, MonadReader c m, MonadState GameState m, MonadIO m)
         => m ()
gameLoop = interpret $ forever gameStep

-- | Starts the game loop.
startGameLoop :: Config -> GameState -> IO ()
startGameLoop c = evalStateT (runReaderT gameLoop c)
