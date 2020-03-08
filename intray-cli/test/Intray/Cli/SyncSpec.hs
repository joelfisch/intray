{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.SyncSpec
  ( spec
  ) where

import TestImport

import qualified Data.Text as T

import Servant.API
import Servant.Client

import Intray.API.Gen ()
import Intray.Client
import Intray.Server.TestUtils

import Intray.Cli.OptParse
import Intray.Cli.Session (loadToken)
import Intray.Cli.Store
import Intray.Cli.TestUtils

spec :: Spec
spec = do
  withIntrayServer $
    it "correctly deletes the local LastSeen after a sync if the item has dissappeared remotely" $ \cenv ->
      forAllValid $ \ti ->
        withValidNewUserAndData cenv $ \un pw _ ->
          withSystemTempDir "intray-cli-test-data" $ \dataDir ->
            withSystemTempDir "intray-cli-test-cache" $ \cacheDir -> do
              let (ClientEnv _ burl _) = cenv
              setEnv "INTRAY_USERNAME" $ T.unpack $ usernameText un
              setEnv "INTRAY_PASSWORD" $ T.unpack pw
              setEnv "INTRAY_URL" $ showBaseUrl burl
              setEnv "INTRAY_CACHE_DIR" $ fromAbsDir cacheDir
              setEnv "INTRAY_DATA_DIR" $ fromAbsDir dataDir
              intray ["login"]
              let sets =
                    Settings
                      { setBaseUrl = Just burl
                      , setUsername = Just un
                      , setCacheDir = cacheDir
                      , setDataDir = dataDir
                      , setSyncStrategy = NeverSync
                      }
              mToken <- runReaderT loadToken sets
              token <-
                case mToken of
                  Nothing -> do
                    expectationFailure "Should have a token after logging in"
                    undefined
                  Just t -> pure t
              uuid <- runClientOrError cenv $ clientPostAddItem token ti
              intray ["sync"]
              intray ["show"]
              mLastSeen1 <- runReaderT readLastSeen sets
              mLastSeen1 `shouldSatisfy` isJust
              NoContent <- runClientOrError cenv $ clientDeleteItem token uuid
              intray ["sync"]
              mLastSeen2 <- runReaderT readLastSeen sets
              mLastSeen2 `shouldSatisfy` isNothing
  let maxFree = 2
  withPaidIntrayServer maxFree $
    it "Can add items past the maximum allowed number of free items locally" $ \cenv ->
      withValidNewUserAndData cenv $ \un pw _ ->
        withSystemTempDir "intray-cli-test-data" $ \dataDir ->
          withSystemTempDir "intray-cli-test-cache" $ \cacheDir -> do
            let (ClientEnv _ burl _) = cenv
            setEnv "INTRAY_USERNAME" $ T.unpack $ usernameText un
            setEnv "INTRAY_PASSWORD" $ T.unpack pw
            setEnv "INTRAY_URL" $ showBaseUrl burl
            setEnv "INTRAY_CACHE_DIR" $ fromAbsDir cacheDir
            setEnv "INTRAY_DATA_DIR" $ fromAbsDir dataDir
            setEnv "INTRAY_SYNC_STRATEGY" "AlwaysSync"
            intray ["login"]
            let sets =
                  Settings
                    { setBaseUrl = Just burl
                    , setUsername = Just un
                    , setCacheDir = cacheDir
                    , setDataDir = dataDir
                    , setSyncStrategy = AlwaysSync
                    }
            let size = runReaderT readClientStoreSize sets
            size `shouldReturn` 0
            intray ["add", "one"]
            size `shouldReturn` 1
            intray ["size"]
            size `shouldReturn` 1
            intray ["add", "two"]
            size `shouldReturn` 2
            intray ["size"]
            size `shouldReturn` 2
            intray ["add", "three"]
            size `shouldReturn` 3
            intray ["size"]
            size `shouldReturn` 3
