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
import Data.Text.Encoding (decodeUtf8)
import Data.Time
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

servePostLogin :: LoginForm -> IntrayHandler (Headers '[ Header "Set-Cookie" Text] NoContent)
servePostLogin LoginForm {..} = do
  me <- runDb $ getBy $ UniqueUsername loginFormUsername
  case me of
    Nothing -> throwError err401
    Just (Entity uid user) ->
      if validatePassword (userHashedPassword user) loginFormPassword
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
                  submittedKey <- parseAccessKeySecretText loginFormPassword
                  if validatePassword accessKeyHashedKey (accessKeySecretText submittedKey)
                    then Just accessKeyPermissions
                    else Nothing
          case msum mli of
            Nothing -> throwError err401
            Just perms -> setLoggedIn uid user perms
  where
    setLoggedIn uid user perms = do
      let cookie =
            AuthCookie {authCookieUserUUID = userIdentifier user, authCookiePermissions = perms}
      IntrayServerEnv {..} <- ask
      mCookie <- liftIO $ makeSessionCookieBS envCookieSettings envJWTSettings cookie
      case mCookie of
        Nothing -> throwError err401
        Just setCookie -> do
          now <- liftIO getCurrentTime
          runDb $ update uid [UserLastLogin =. Just now]
          return $ addHeader (decodeUtf8 setCookie) NoContent
