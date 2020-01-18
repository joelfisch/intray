module Intray.CliSpec
  ( spec
  ) where

import TestImport

import Servant.Client

import Intray.Cli.TestUtils
import Intray.Server.TestUtils

spec :: Spec
spec =
  withIntrayServer $
  it "Going through the usual manual steps 'just works'" $ \(ClientEnv _ burl _) ->
    withSystemTempDir "intray-cli-test-cache" $ \cacheDir ->
      withSystemTempDir "intray-cli-test-data" $ \dataDir -> do
        intray
          [ "register"
          , "--username"
          , "testuser"
          , "--password"
          , "testpass"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        intray
          [ "login"
          , "--username"
          , "testuser"
          , "--password"
          , "testpass"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        intray
          [ "add"
          , "hello"
          , "world"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        intray
          [ "show"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        intray
          [ "done"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        intray
          [ "size"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        intray
          [ "sync"
          , "--url"
          , showBaseUrl burl
          , "--cache-dir"
          , fromAbsDir cacheDir
          , "--data-dir"
          , fromAbsDir dataDir
          ]
        intray ["logout", "--cache-dir", fromAbsDir cacheDir, "--data-dir", fromAbsDir dataDir]
