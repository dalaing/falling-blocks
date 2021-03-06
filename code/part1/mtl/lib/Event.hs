{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PatternSynonyms       #-}
{- |
The 'Event' module provides events specific to our application.
-}
module Event (
    AppEvent(..)
  , toAppEvent
  , handleEvent
  -- * Interface to SDL
  , doEvents
  ) where

-- from 'base'
import           Data.Foldable            (traverse_)

-- from 'mtl'
import           Control.Monad.State      (MonadState, modify)
import           Control.Monad.Trans      (MonadIO, liftIO)

-- from 'sdl2'
import           SDL.Event                (Event (..), EventPayload (..),
                                           InputMotion (..),
                                           KeyboardEventData (..), pollEvents)
import           SDL.Input.Keyboard       (Keysym (..))
import           SDL.Input.Keyboard.Codes (pattern KeycodeQ, pattern KeycodeSpace)

-- internal
import           Colour                   (nextColour)
import           GameState                (GameState (..))

-- | The 'AppEvent' type.
data AppEvent =
    AppCycle        -- ^ Cycle event
  | AppQuit         -- ^ Quit event
  | AppOther Event  -- ^ Any SDL event we don't handle
  deriving (Eq, Show)

-- | Converts an SDL event to an 'AppEvent'.
toAppEvent :: Event
           -> AppEvent
toAppEvent event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then
        case keysymKeycode (keyboardEventKeysym keyboardEvent) of
          KeycodeSpace -> AppCycle
          KeycodeQ     -> AppQuit
          _            -> AppOther event
      else
        AppOther event
    _ -> AppOther event

-- | Update the 'GameState' based on an 'AppEvent'.
handleEvent :: MonadState GameState m
            => AppEvent
            -> m ()
handleEvent AppCycle = modify $ \gs -> gs { backgroundColour = nextColour . backgroundColour $ gs }
handleEvent AppQuit = modify $ \gs -> gs { hasQuit = True }
handleEvent _ = return ()

-- | Update the 'GameState' by fetching the pending SDL events and
-- processing them.
doEvents :: (MonadState GameState m, MonadIO m)
         => m ()
doEvents = do
  es <- liftIO pollEvents
  traverse_ (handleEvent . toAppEvent) es
