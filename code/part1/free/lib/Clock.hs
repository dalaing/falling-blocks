{-# LANGUAGE TemplateHaskell #-}
module Clock (
    Clock
  , newClock
  , startClock
  , stopClock
  , waitForClock
  , linkClocks
  , unlinkClocks
  , ClockWatcher
  , newClockWatcher
  , watch
  ) where

-- from 'base'
import Data.Word (Word32)
import Control.Monad (when, forever)
import Control.Concurrent (forkIO, killThread, ThreadId)

-- from 'mtl'
import Control.Monad.Trans (MonadIO, liftIO)

-- from 'stm'
import Control.Concurrent.STM (atomically, retry)
import Control.Concurrent.STM.TVar (TVar, newTVar, modifyTVar, readTVar, writeTVar)

-- from 'lens'
import Control.Lens.TH (makeLenses)
import Control.Lens.Getter ((^.))

-- from 'sdl2'
import SDL.Time (Timer, addTimer, removeTimer, RetriggerTimer(..))

data Clock = Clock {
    _tick :: TVar Int
  }

makeLenses ''Clock

newClock :: MonadIO m => m Clock
newClock = liftIO . atomically $ 
  Clock <$> newTVar 0

startClock :: MonadIO m => Clock -> Word32 -> m Timer
startClock clock interval = liftIO . addTimer interval . timerCallback $ clock

timerCallback :: Clock -> Word32 -> IO RetriggerTimer
timerCallback c interval = do
  atomically $ modifyTVar (c ^. tick) (+ 1)
  return $ Reschedule interval

stopClock :: MonadIO m => Timer -> m Bool
stopClock = removeTimer

waitForClock :: MonadIO m => Clock -> m ()
waitForClock clock = liftIO $ do
  old <- atomically . readTVar $ clock ^. tick
  atomically $ do
    new <- readTVar $ clock ^. tick
    when (old == new) retry

linkClocks :: MonadIO m
           => Int
           -> Clock
           -> Clock
           -> m ThreadId
linkClocks period parent child = liftIO . forkIO . forever . atomically $ do
  pt <- readTVar (parent ^. tick)
  let (d, m) = pt `divMod` period
  if m == 0
  then writeTVar (child ^. tick) d  
  else retry

unlinkClocks :: MonadIO m
             => ThreadId
             -> m ()
unlinkClocks = liftIO . killThread

data ClockWatcher = ClockWatcher {
    _parentTick   :: TVar Int
  , _factor       :: Int
  , _lastTickSeen :: TVar Int
  }

makeLenses ''ClockWatcher

newClockWatcher :: MonadIO m
                => Clock 
                -> Int 
                -> m ClockWatcher
newClockWatcher (Clock t) f = liftIO . atomically $ 
    ClockWatcher t f <$> newTVar 0

watch :: MonadIO m
          => ClockWatcher
          -> m Bool
watch cw = liftIO . atomically $ do
  pt <- readTVar (cw ^. parentTick)
  lts <- readTVar (cw ^. lastTickSeen)
  let result = lts + cw ^. factor < pt
  when result $
    writeTVar (cw ^. lastTickSeen) pt
  return result
