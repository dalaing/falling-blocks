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
import           SDL.Input.Keyboard.Codes (pattern KeycodeQ, pattern KeycodeSpace)

-- internal
import           Colour                   (nextColour)
import           GameState                (GameState (..))

-- | The 'AppEvent' type.
data AppEvent =
  -- | Cycle event
    AppCycle
  -- | Quit event
  | AppQuit
  -- | Any SDL event we don't handle
  | AppOther Event
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
handleEvent :: GameState
            -> AppEvent
            -> GameState
handleEvent gs AppCycle = gs { backgroundColour = nextColour . backgroundColour $ gs }
handleEvent gs AppQuit = gs { hasQuit = True }
handleEvent gs _ = gs

-- | Update the 'GameState' by fetching the pending SDL events and
-- processing them.
doEvents :: GameState
         -> IO GameState
doEvents gs = do
  es <- pollEvents
  return $ foldr (flip handleEvent . toAppEvent) gs es
