{-# LANGUAGE LambdaCase #-}

module Control.Concurrent.FreshVarSpec
  ( spec
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.FreshVar
import Control.Monad (replicateM)
import Data.IORef (atomicModifyIORef, newIORef, readIORef)
import Test.Hspec

spec :: Spec
spec = describe "readFreshVar" $ do
  it "can read a value" $ do
    var <- newFreshVar alwaysFresh onRefreshFalse
    readFreshVar var `shouldReturn` True

  it "can refresh a stale value" $ do
    var <- newFreshVar alwaysStale onRefreshFalse
    readFreshVar var `shouldReturn` False

  it "can preemptively refresh a stale value" $ do
    var <- newPreemptiveFreshVar alwaysFresh alwaysStale onRefreshFalse
    -- Value before async refresh
    readFreshVar var `shouldReturn` True
    threadDelay 1
    -- Value after async refresh
    readFreshVar var `shouldReturn` False

  it "only refreshes once without blocking reads" $ do
    count <- newIORef (0 :: Int)
    var <- newPreemptiveFreshVar alwaysFresh pure $ \case
      Nothing -> pure True
      Just _ -> do
        atomicModifyIORef count $ \x -> (x + 1, ())
        threadDelay 10
        pure False
    vals <- replicateM 3 $ readFreshVar var
    threadDelay 30
    vals `shouldBe` replicate 3 True
    readIORef count `shouldReturn` 1
    readFreshVar var `shouldReturn` False
    threadDelay 20
    readIORef count `shouldReturn` 2

alwaysFresh :: a -> IO Bool
alwaysFresh = const $ pure False

alwaysStale :: a -> IO Bool
alwaysStale = const $ pure True

onRefreshFalse :: Applicative f => Maybe a -> f Bool
onRefreshFalse = \case
  Just _ -> pure False
  Nothing -> pure True
