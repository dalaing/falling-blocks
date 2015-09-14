{-# LANGUAGE OverloadedStrings #-}
module Main (
    main
  ) where

-- from 'sdl2'
import           SDL.Init           (InitFlag (..), initialize)
import           SDL.Video          (createRenderer, createWindow,
                                     defaultWindow)
import           SDL.Video.Renderer (defaultRenderer)

-- internal
import           GameLoop           (gameLoop)
import           GameState          (defaultGameState)

main :: IO ()
main = do
  initialize [InitEverything]
  window <- createWindow "My SDL Application" defaultWindow
  r <- createRenderer window (-1) defaultRenderer
  gameLoop r defaultGameState
