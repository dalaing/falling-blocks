{-# LANGUAGE TemplateHaskell #-}
module Config (
    Config
  , mkConfig
  , HasConfig(..)
  ) where

-- from 'lens'
import           Control.Lens       (Lens')
import           Control.Lens.TH    (makeClassy)

-- from 'sdl2'
import           SDL.Video.Renderer (Renderer)

data Config = Config {
    _renderer :: Renderer
  } deriving (Eq, Show)

makeClassy ''Config

mkConfig :: Renderer -> Config
mkConfig = Config
