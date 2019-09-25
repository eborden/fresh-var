{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}

-- | A 'FreshVar' is a shared readable value that always remains fresh. One can
-- be created and read simply by providing a stale check and a create/refresh
-- action.
--
-- >>> var <- newFreshVar alwaysFresh onRefreshFalse
-- >>> readFreshVar var
-- True
--
-- When a value loses freshness the create/refresh action is run to refresh it.
--
-- >>> var <- newFreshVar alwaysStale onRefreshFalse
-- >>> readFreshVar var
-- False
--
-- Refreshes can also be preemptively triggered by providing an early warning
-- check. These refreshes happen async and can prevent excessive blocking
-- when values become stale.
--
-- >>> import Control.Concurrent (threadDelay)
-- >>>
-- >>> var <- newPreemptiveFreshVar alwaysFresh alwaysStale onRefreshFalse
-- >>> readFreshVar var -- True
-- >>> threadDelay 1
-- >>> readFreshVar var
-- False

module Control.Concurrent.FreshVar
  ( FreshVar
  , newFreshVar
  , newPreemptiveFreshVar
  , readFreshVar
  )
where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, takeMVar, tryTakeMVar, withMVar)
import Control.Exception (bracket, finally, mask_)
import Control.Monad (void)
import Data.Foldable (for_)

-- $setup
-- >>> let alwaysFresh = const $ pure False
-- >>> let alwaysStale = const $ pure True
-- >>> let onRefreshFalse = pure . maybe True (const False)

-- | A value that is always fresh
newtype FreshVar a = FreshVar { getFreshMVar :: MVar (Fresh a) }

data Fresh a = Fresh
  { getFresh :: a
  , refreshMutex :: MVar ()
  , isNearingStale :: a -> IO Bool
  , isStale :: a -> IO Bool
  , create :: Maybe a -> IO a
  }

-- | Create a value that will always remain fresh
--
-- A 'FreshVar' will refresh itself when its stale check returns 'True'. These
-- refreshes are done lazily and occur when a stale value is read via
-- 'readFreshVar'.
--
newFreshVar
  :: (a -> IO Bool) -- ^ A check to determine if the value is stale
  -> (Maybe a -> IO a) -- ^ A procedure to create or refresh the value
  -> IO (FreshVar a)
newFreshVar staleCheck = newPreemptiveFreshVar staleCheck (const $ pure False)

-- | Create a 'FreshVar' that preemptively refreshes itself
--
-- A 'FreshVar' will block reads when the value becomes stale. However a
-- preemptive 'FreshVar' can refresh itself before the value becomes stale
-- and prevent blocking reads.
--
newPreemptiveFreshVar
  :: (a -> IO Bool) -- ^ A check to determine if the value is stale
  -> (a -> IO Bool) -- ^ A check to determine if that value is nearing stale
  -> (Maybe a -> IO a) -- ^ A procedure to create or refresh the value
  -> IO (FreshVar a)
newPreemptiveFreshVar isStale isNearingStale create = do
  getFresh <- create Nothing
  refreshMutex <- newMVar ()
  FreshVar <$> newMVar Fresh
    { getFresh
    , refreshMutex
    , isNearingStale
    , isStale
    , create
    }

-- | Read a value and ensure it is always fresh
readFreshVar :: Show a => FreshVar a -> IO a
readFreshVar v = fmap getFresh $ modifyFreshVar v $ \fresh -> do
  nearStale <- isNearingStale fresh $ getFresh fresh
  if nearStale
    then do
      void . forkIO $ tryRefresh v
      pure fresh
    else do
      stale <- isStale fresh $ getFresh fresh
      if stale then syncRefresh fresh else pure fresh

-- | Refresh a value and block if the mutex is held by another thread
syncRefresh :: Fresh a -> IO (Fresh a)
syncRefresh t = withMVar (refreshMutex t) (const $ refresh t)

-- | Attempt to refresh a value, but do nothing if another thread is already refreshing
tryRefresh :: FreshVar a -> IO ()
tryRefresh v = tryWithMutex v . void $ modifyFreshVar v refresh

refresh :: Fresh a -> IO (Fresh a)
refresh t = do
  x <- create t . Just $ getFresh t
  pure $ t { getFresh = x }

-- | Attempt to lock mutation on a 'Fresh'
tryWithMutex :: FreshVar a -> IO () -> IO ()
tryWithMutex v f = mask_ $ do
  (mutexVar, mayMutex) <- bracket takeMutex putFresh unwrapMutex
  for_ mayMutex $ \() -> f `finally` putMVar mutexVar ()
 where
  takeMutex = do
    fresh <- takeMVar freshMVar
    let mutexVar = refreshMutex fresh
    mayMutex <- tryTakeMVar mutexVar
    pure (fresh, mutexVar, mayMutex)
  putFresh (fresh, _, _) = putMVar freshMVar fresh
  unwrapMutex (_, mutexVar, mayMutex) = pure (mutexVar, mayMutex)
  freshMVar = getFreshMVar v

modifyFreshVar :: FreshVar a -> (Fresh a -> IO (Fresh a)) -> IO (Fresh a)
modifyFreshVar v f = modifyMVar (getFreshMVar v) $ fmap dup . f
  where dup x = (x, x)
