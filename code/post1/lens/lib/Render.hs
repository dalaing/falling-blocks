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
import           Control.Monad.Trans  (MonadIO, liftIO)

-- from 'linear'
import           Linear               (V4 (..))

-- from 'sdl2'
import           SDL                  (($=))
import           SDL.Video.Renderer   (Renderer, clear, present,
                                       rendererDrawColor)

-- | Render the background.
background :: (MonadReader Renderer m, MonadIO m)
           => m ()
background = do
  r <- ask
  liftIO $ do
    rendererDrawColor r $= V4 0 0 255 255
    clear r

-- | Render the game.
render :: (MonadReader Renderer m, MonadIO m)
       => m ()
render = do
  background
  r <- ask
  liftIO $
    present r
