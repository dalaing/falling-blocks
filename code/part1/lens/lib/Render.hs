{-# LANGUAGE FlexibleContexts      #-}
{- |
The 'Render' module contains the functions used to render the game.

The 'render' function is the main entry point.
-}
module Render (
    render
  ) where

-- from 'mtl'
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.State  (MonadState)
import           Control.Monad.Trans  (MonadIO, liftIO)

-- from 'lens'
import           Control.Lens.Getter  (to, use, view)

-- from 'sdl2'
import           SDL                  (($=))
import           SDL.Video.Renderer   (clear, present, rendererDrawColor)

-- internal
import           Colour               (toSDLColour)
import           Config               (HasConfig (..))
import           GameState            (GameState, backgroundColour)

-- | Render the background.
background :: (HasConfig c, MonadReader c m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- view renderer
  c <- use $ backgroundColour . to toSDLColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r

-- | Render the game.
render :: (HasConfig c, MonadReader c m, MonadState GameState m, MonadIO m)
       => m ()
render = do
  background
  r <- view renderer
  liftIO $
    present r
