{-# LANGUAGE LambdaCase #-}

module Control.Concurrent.FreshVarSpec
  ( spec
  )
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.FreshVar
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

alwaysFresh :: a -> Bool
alwaysFresh = const False

alwaysStale :: a -> Bool
alwaysStale = const True

onRefreshFalse :: Applicative f => Maybe a -> f Bool
onRefreshFalse = \case
  Just _ -> pure False
  Nothing -> pure True
