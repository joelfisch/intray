{-# LANGUAGE OverloadedStrings #-}

module Intray.Cli.NoSyncSpec
  ( spec
  ) where

import TestImport

import Intray.Cli
import Intray.Cli.OptParse

spec :: Spec
spec = do
  it "Works fine without a server" $
    withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
      withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing
                , setUsername = Nothing
                , setCacheDir = cacheDir
                , setDataDir = dataDir
                , setSyncStrategy = NeverSync
                }
        let intray d = runReaderT (dispatch d) sets
        intray $ DispatchPostPostAddItem "hello world"
        intray DispatchShowItem
        intray DispatchDoneItem
        intray DispatchSize
  specify "login fails immediately if no server is configured" $
    withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
      withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing
                , setUsername = Nothing
                , setCacheDir = cacheDir
                , setDataDir = dataDir
                , setSyncStrategy = NeverSync
                }
        let intray d = runReaderT (dispatch d) sets
        intray
          (DispatchLogin LoginSettings {loginSetUsername = Nothing, loginSetPassword = Nothing}) `shouldThrow`
          (== ExitFailure 1)
