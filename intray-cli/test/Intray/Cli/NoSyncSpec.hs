{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Intray.Cli.NoSyncSpec
  ( spec
  ) where

import TestImport

import Intray.Cli
import Intray.Cli.OptParse
import Intray.Data

spec :: Spec
spec = do
  it "correctly errors when a user tries to register but has no server configured" $
    withSystemTempDir "intray-cli-test-data" $ \dataDir ->
      withSystemTempDir "intray-cli-test-cache" $ \cacheDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing
                , setUsername = Nothing
                , setCacheDir = cacheDir
                , setDataDir = dataDir
                , setSyncStrategy = NeverSync
                }
        let intray d = runReaderT (dispatch d) sets
        let rs =
              RegisterSettings
                { registerSetUsername = parseUsername "testuser"
                , registerSetPassword = Just "password"
                }
        intray (DispatchRegister rs) `shouldThrow` (\(_ :: ExitCode) -> True)
  it "correctly errors when a user tries to login but has no server configured" $
    withSystemTempDir "intray-cli-test-data" $ \dataDir ->
      withSystemTempDir "intray-cli-test-cache" $ \cacheDir -> do
        let sets =
              Settings
                { setBaseUrl = Nothing
                , setUsername = Nothing
                , setCacheDir = cacheDir
                , setDataDir = dataDir
                , setSyncStrategy = NeverSync
                }
        let intray d = runReaderT (dispatch d) sets
        let rs =
              LoginSettings
                {loginSetUsername = parseUsername "testuser", loginSetPassword = Just "password"}
        intray (DispatchLogin rs) `shouldThrow` (\(_ :: ExitCode) -> True)
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
