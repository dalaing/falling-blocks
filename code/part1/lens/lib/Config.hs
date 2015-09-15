{-# LANGUAGE TemplateHaskell #-}
-- | The 'Config' module contains the configuration data for our
-- application and the associated functions.
module Config (
    Config
  , newConfig
  , HasConfig(..)
  ) where

-- from 'lens'
import           Control.Lens.TH    (makeClassy)

-- from 'sdl2'
import           SDL.Video.Renderer (Renderer)

-- | The configuration data for our application
data Config = Config {
    _renderer :: Renderer   -- ^ The SDL renderer
  } deriving (Eq, Show)

makeClassy ''Config

-- | Creates a new 'Config' value
newConfig :: Renderer 
          -> Config
newConfig = Config
