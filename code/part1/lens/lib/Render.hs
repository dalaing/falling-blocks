{-# LANGUAGE FlexibleContexts      #-}
{- |
The 'Render' module contains the functions used to render the game.

The 'render' function is the main entry point.
-}
module Render (
    render
  ) where

-- from 'mtl'
import           Control.Monad.Reader (MonadReader, ask)
import           Control.Monad.State  (MonadState)
import           Control.Monad.Trans  (MonadIO, liftIO)

-- from 'lens'
import           Control.Lens.Getter  (to, use)

-- from 'sdl2'
import           SDL                  (($=))
import           SDL.Video.Renderer   (Renderer, clear, present,
                                       rendererDrawColor)

-- internal
import           Colour               (toSDLColour)
import           GameState            (GameState, backgroundColour)

-- | Render the background.
background :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
           => m ()
background = do
  r <- ask
  c <- use $ backgroundColour . to toSDLColour
  liftIO $ do
    rendererDrawColor r $= c
    clear r

-- | Render the game.
render :: (MonadReader Renderer m, MonadState GameState m, MonadIO m)
       => m ()
render = do
  background
  r <- ask
  liftIO $
    present r
