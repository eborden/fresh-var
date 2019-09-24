{-# LANGUAGE LambdaCase #-}
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
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, putMVar, tryTakeMVar, withMVar)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Foldable (traverse_)

-- $setup
-- >>> let alwaysFresh = const False
-- >>> let alwaysStale = const True
-- >>> let onRefreshFalse = pure . maybe True (const False)

-- | A value that is always fresh
newtype FreshVar a = FreshVar { getFreshMVar :: MVar (Fresh a) }

data Fresh a = Fresh
  { getFresh :: a
  , refreshMutex :: MVar ()
  , isNearingStale :: a -> Bool
  , isStale :: a -> Bool
  , create :: Maybe a -> IO a
  }

-- | Create a value that will always remain fresh
--
-- A 'FreshVar' will refresh itself when its stale check returns 'True'. These
-- refreshes are done lazily and occur when a stale value is read via
-- 'readFreshVar'.
--
newFreshVar
  :: (a -> Bool) -- ^ A check to determine if the value is stale
  -> (Maybe a -> IO a) -- ^ A procedure to create or refresh the value
  -> IO (FreshVar a)
newFreshVar staleCheck = newPreemptiveFreshVar staleCheck (const False)

-- | Create a 'FreshVar' that preemptively refreshes itself
--
-- A 'FreshVar' will block reads when the value becomes stale. However a
-- preemptive 'FreshVar' can refresh itself before the value becomes stale
-- and prevent blocking reads.
--
newPreemptiveFreshVar
  :: (a -> Bool) -- ^ A check to determine if the value is stale
  -> (a -> Bool) -- ^ A check to determine if that value is nearing stale
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
readFreshVar v = fmap getFresh $ modifyFreshVar v $ \fresh -> if
  | isNearingStale fresh $ getFresh fresh -> do
    void . forkIO $ tryRefresh v
    pure fresh
  | isStale fresh $ getFresh fresh -> syncRefresh fresh
  | otherwise -> pure fresh

-- | Refresh a value and block if the mutex is held by another thread
syncRefresh :: Fresh a -> IO (Fresh a)
syncRefresh t = withMVar (refreshMutex t) (const $ refresh t)

-- | Attempt to refresh a value, but do nothing if another thread is already refreshing
tryRefresh :: FreshVar a -> IO ()
tryRefresh v = void . modifyFreshVar v $ \t -> tryWithMutex t (refresh t)

refresh :: Fresh a -> IO (Fresh a)
refresh t = do
  x <- create t . Just $ getFresh t
  pure $ t { getFresh = x }

-- | Attempt to lock mutation on a 'Fresh'
tryWithMutex :: Fresh a -> IO (Fresh a) -> IO (Fresh a)
tryWithMutex t f = with $ \case
  Nothing -> pure t -- do nothing when we don't have a lock
  Just () -> f -- run the action when we've taken the lock
 where
  mutex = refreshMutex t
  -- bracket to prevent indefinitely locking the mutext on exception
  with = bracket (tryTakeMVar mutex) (traverse_ (putMVar mutex))

modifyFreshVar :: FreshVar a -> (Fresh a -> IO (Fresh a)) -> IO (Fresh a)
modifyFreshVar v f = modifyMVar (getFreshMVar v) $ fmap dup . f
  where dup x = (x, x)
