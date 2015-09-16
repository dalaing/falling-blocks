{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
  ) where

-- from 'base'
import           Control.Monad       (void)

-- from 'mtl'
import           Control.Monad.Trans (liftIO)

-- from 'sdl2'
import           SDL.Init           (InitFlag (..), initialize)
import           SDL.Video          (createRenderer, createWindow,
                                     defaultWindow)
import           SDL.Video.Renderer (defaultRenderer)

-- internal
import           Config             (Config(..))
import           Clock              (newClock, startClock, stopClock, newClockWatcher)
import           GameLoop           (startGameLoop)
import           GameState          (defaultGameState)

main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  r <- createRenderer window (-1) defaultRenderer
  uc <- liftIO newClock
  lc <- liftIO $ newClockWatcher uc 40
  t <- startClock uc 50
  startGameLoop (Config r uc lc) defaultGameState
  void $ stopClock t
