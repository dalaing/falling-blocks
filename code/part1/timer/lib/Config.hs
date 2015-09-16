{-# LANGUAGE TemplateHaskell #-}
-- | The 'Config' module contains the configuration data for our
-- application and the associated functions.
module Config (
    Config(..)
  , HasConfig(..)
  ) where

-- from 'lens'
import           Control.Lens.TH     (makeClassy)

-- from 'sdl2'
import           SDL.Video.Renderer  (Renderer)

-- internal
import           Clock               (Clock, ClockWatcher)

-- | The configuration data for our application
data Config = Config {
    _renderer   :: Renderer         -- ^ The SDL renderer
  , _uiClock    :: Clock            -- ^ The UI update clock
  , _logicClock :: ClockWatcher     -- ^ The logic update clock
  }

makeClassy ''Config
