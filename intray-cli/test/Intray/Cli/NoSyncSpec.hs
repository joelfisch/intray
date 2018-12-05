{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Intray.Cli.NoSyncSpec
    ( spec
    ) where

import TestImport

import Intray.Cli
import Intray.Cli.OptParse

spec :: Spec
spec =
    around (withSystemTempDir "intray-cli-test") $ do
        it "Works fine without a server" $ \tdir -> do
            let sets =
                    Settings
                        { setBaseUrl = Nothing
                        , setUsername = Nothing
                        , setIntrayDir = tdir
                        , setSyncStrategy = NeverSync
                        }
            let intray d = runReaderT (dispatch d) sets
            intray $ DispatchPostPostAddItem "hello world"
            intray DispatchShowItem
            intray DispatchDoneItem
            intray DispatchSize
        specify "login fails immediately if no server is configured" $ \tdir -> do
            let sets =
                    Settings
                        { setBaseUrl = Nothing
                        , setUsername = Nothing
                        , setIntrayDir = tdir
                        , setSyncStrategy = NeverSync
                        }
            let intray d = runReaderT (dispatch d) sets
            intray
                (DispatchLogin
                     LoginSettings
                         { loginSetUsername = Nothing
                         , loginSetPassword = Nothing
                         }) `shouldThrow`
                (== ExitFailure 1)
