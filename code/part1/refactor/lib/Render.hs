{- |
The 'Render' module contains the functions used to render the game.

The 'render' function is the main entry point.
-}
module Render (
    render
  ) where

-- from 'linear'
import           Linear             (V4 (..))

-- from 'sdl2'
import           SDL                (($=))
import           SDL.Video.Renderer (Renderer, clear, present,
                                     rendererDrawColor)

-- | Render the background.
background :: Renderer
           -> IO ()
background r = do
  rendererDrawColor r $= V4 0 0 255 255
  clear r

-- | Render the game.
render :: Renderer
       -> IO ()
render r = do
  background r
  present r
