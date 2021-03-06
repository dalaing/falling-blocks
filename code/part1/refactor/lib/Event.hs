{-# LANGUAGE PatternSynonyms #-}
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

-- from 'sdl2'
import           SDL.Event                (Event (..), EventPayload (..),
                                           InputMotion (..),
                                           KeyboardEventData (..), pollEvents)
import           SDL.Input.Keyboard       (Keysym (..))
import           SDL.Input.Keyboard.Codes (pattern KeycodeQ)

-- internal
import           GameState                (GameState (..))

-- | The 'AppEvent' type.
data AppEvent =
    AppQuit         -- ^ Quit event
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
          KeycodeQ -> AppQuit
          _ -> AppOther event
      else
        AppOther event
    _ -> AppOther event

-- | Update the 'GameState' based on an 'AppEvent'.
handleEvent :: AppEvent
            -> GameState
            -> GameState
handleEvent AppQuit gs = gs { hasQuit = True }
handleEvent  _ gs = gs

-- | Update the 'GameState' by fetching the pending SDL events and
-- processing them.
doEvents :: GameState
         -> IO GameState
doEvents gs = do
  es <- pollEvents
  return $ foldr (handleEvent . toAppEvent) gs es
