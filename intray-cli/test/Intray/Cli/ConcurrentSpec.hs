module Intray.Cli.ConcurrentSpec
  ( spec
  ) where

import Control.Concurrent.Async
import Intray.Cli.OptParse
import Intray.Cli.Store
import Intray.Cli.TestUtils
import Intray.Server.TestUtils
import TestImport

spec :: Spec
spec =
  withIntrayServer $
  it
    "Going through the usual manual steps just works, even if multiple clients do it at the same time in the same place" $ \(ClientEnv _ burl _) ->
    withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
      withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
        setEnv "INTRAY_USERNAME" "testuser"
        setEnv "INTRAY_PASSWORD" "testpassword"
        setEnv "INTRAY_URL" $ showBaseUrl burl
        setEnv "INTRAY_CACHE_DIR" $ fromAbsDir cacheDir
        setEnv "INTRAY_DATA_DIR" $ fromAbsDir dataDir
        intray ["register"]
        intray ["login"]
        let additions = 2
        replicateConcurrently_ additions $ intray ["add", "hello", "world"]
        let sets =
              Settings
                { setBaseUrl = Just burl
                , setCacheDir = cacheDir
                , setDataDir = dataDir
                , setSyncStrategy = NeverSync
                }
        s <- runReaderT readClientStoreSize sets
        s `shouldBe` additions
