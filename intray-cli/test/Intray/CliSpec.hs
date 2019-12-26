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
  aroundWith (\adFunc a -> withSystemTempDir "intray-cli-test" (\d -> adFunc (a, fromAbsDir d))) $
  it "Going through the usual manual steps 'just works'" $ \(ClientEnv _ burl _, tdir) -> do
    intray
      [ "register"
      , "--username"
      , "testuser"
      , "--password"
      , "testpass"
      , "--url"
      , showBaseUrl burl
      , "--intray-dir"
      , tdir
      ]
    intray
      [ "login"
      , "--username"
      , "testuser"
      , "--password"
      , "testpass"
      , "--url"
      , showBaseUrl burl
      , "--intray-dir"
      , tdir
      ]
    intray ["add", "hello", "world", "--url", showBaseUrl burl, "--intray-dir", tdir]
    intray ["show", "--url", showBaseUrl burl, "--intray-dir", tdir]
    intray ["done", "--url", showBaseUrl burl, "--intray-dir", tdir]
    intray ["size", "--url", showBaseUrl burl, "--intray-dir", tdir]
    intray ["sync", "--url", showBaseUrl burl, "--intray-dir", tdir]
    intray ["logout", "--intray-dir", tdir]
