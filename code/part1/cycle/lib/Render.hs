{- |
The 'Render' module contains the functions used to render the game.

The 'render' function is the main entry point.
-}
module Render (
    render
  ) where

-- from 'sdl2'
import           SDL                (($=))
import           SDL.Video.Renderer (Renderer, clear, present,
                                     rendererDrawColor)

-- internal
import           Colour             (toSDLColour)
import           GameState          (GameState (..))

-- | Render the background.
background :: Renderer
           -> GameState
           -> IO ()
background r s = do
  rendererDrawColor r $= (toSDLColour . backgroundColour $ s)
  clear r

-- | Render the game.
render :: Renderer
       -> GameState
       -> IO ()
render r s = do
  background r s
  present r
