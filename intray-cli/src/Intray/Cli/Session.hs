module Intray.Cli.Session
    ( withToken
    , loadToken
    , loadSession
    , saveSession
    ) where

import Import

import qualified Data.ByteString as SB
import qualified Data.ByteString.Builder as SBB
import qualified Data.ByteString.Lazy as LB

import Web.Cookie

import Servant.Auth.Client
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.Cli.OptParse
import Intray.Cli.Path

withToken :: (Token -> CliM a) -> CliM a
withToken func = do
    mToken <- loadToken
    case mToken of
        Nothing -> liftIO $ die "Please log in first"
        Just token -> func token

loadToken :: CliM (Maybe Token)
loadToken = do
    mCookie <- loadSession
    pure $ (Token . setCookieValue) <$> mCookie

loadSession :: CliM (Maybe SetCookie)
loadSession = do
    p <- sessionPath
    mContents <- liftIO $ forgivingAbsence $ SB.readFile $ toFilePath p
    pure $ parseSetCookie <$> mContents

saveSession :: SetCookie -> CliM ()
saveSession setCookie = do
    p <- sessionPath
    liftIO $ do
        ensureDir $ parent p
        LB.writeFile (toFilePath p) $
            SBB.toLazyByteString $ renderSetCookie setCookie
