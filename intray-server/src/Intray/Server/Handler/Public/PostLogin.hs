{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.Public.PostLogin
    ( servePostLogin
    ) where

import Import

import Control.Monad.Except
import qualified Data.Text.Encoding as TE
import Data.Time
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils

servePostLogin ::
       LoginForm
    -> IntrayHandler (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)
servePostLogin LoginForm {..} = do
    me <- runDb $ getBy $ UniqueUsername loginFormUsername
    case me of
        Nothing -> throwError err401
        Just (Entity uid user) ->
            if validatePassword
                   (userHashedPassword user)
                   (TE.encodeUtf8 loginFormPassword)
                then do
                    admins <- asks envAdmins
                    let perms =
                            if userUsername user `elem` admins
                                then adminPermissions
                                else userPermissions
                    setLoggedIn uid user perms
                else do
                    aks <- runDb $ selectList [] [Asc AccessKeyCreatedTimestamp]
                    let mli =
                            flip map aks $ \(Entity _ AccessKey {..}) -> do
                                submittedKey <-
                                    parseAccessKeySecretText loginFormPassword
                                if validatePassword
                                       accessKeyHashedKey
                                       (TE.encodeUtf8 $
                                        accessKeySecretText submittedKey)
                                    then Just accessKeyPermissions
                                    else Nothing
                    case msum mli of
                        Nothing -> throwError err401
                        Just perms -> setLoggedIn uid user perms
  where
    setLoggedIn uid user perms = do
        let cookie =
                AuthCookie
                { authCookieUserUUID = userIdentifier user
                , authCookiePermissions = perms
                }
        IntrayServerEnv {..} <- ask
        mApplyCookies <-
            liftIO $ acceptLogin envCookieSettings envJWTSettings cookie
        case mApplyCookies of
            Nothing -> throwError err401
            Just applyCookies -> do
                now <- liftIO getCurrentTime
                runDb $ update uid [UserLastLogin =. Just now]
                return $ applyCookies NoContent
