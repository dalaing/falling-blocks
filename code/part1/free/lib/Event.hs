{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE DeriveFunctor         #-}
{- |
The 'Event' module provides events specific to our application.
-}
module Event (
    AppEvent(..)
  , cycleEvent
  , renderEvent
  , timerEvent
  , quitEvent
  -- * Interface to SDL
  , doEvents
  ) where

-- from 'base'
import           Data.Foldable            (traverse_)

-- from 'mtl'
import           Control.Monad.Trans      (MonadIO, liftIO)

-- from 'free'
import           Control.Monad.Trans.Free (MonadFree, liftF)

-- from 'sdl2'
import           SDL.Event                (Event (..), EventPayload (..),
                                           InputMotion (..),
                                           KeyboardEventData (..), pollEvents)
import           SDL.Input.Keyboard       (Keysym (..))
import           SDL.Input.Keyboard.Codes (pattern KeycodeQ, pattern KeycodeSpace)

-- | The 'AppEvent' type.
data AppEvent k =
    AppCycle k      -- ^ Cycle event
  | AppRender k     -- ^ Render event
  | AppTimer k      -- ^ Timer event
  | AppQuit         -- ^ Quit event
  deriving (Eq, Show, Functor)

cycleEvent :: MonadFree AppEvent m
           => m ()
cycleEvent = liftF $ AppCycle ()

renderEvent :: MonadFree AppEvent m
           => m ()
renderEvent = liftF $ AppRender ()

timerEvent :: MonadFree AppEvent m
           => m ()
timerEvent = liftF $ AppTimer ()

quitEvent :: MonadFree AppEvent m
           => m ()
quitEvent = liftF $ AppQuit

toAppEvent :: MonadFree AppEvent m
           => Event
           -> m ()
toAppEvent event =
  case eventPayload event of
    KeyboardEvent keyboardEvent ->
      if keyboardEventKeyMotion keyboardEvent == Pressed
      then
        case keysymKeycode (keyboardEventKeysym keyboardEvent) of
          KeycodeSpace -> cycleEvent
          KeycodeQ     -> quitEvent
          _            -> return ()
      else
        return ()
    _ -> return ()

doEvents :: (MonadFree AppEvent m, MonadIO m)
         => m ()
doEvents = do
  -- grab the SDL events that have happened while we were waiting
  es <- liftIO pollEvents
  traverse_ toAppEvent es
