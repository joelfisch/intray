{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.Public.PostRegister
    ( servePostRegister
    ) where

import Import

import qualified Data.ByteString.Lazy as LB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Types

import Intray.Server.Handler.Utils

servePostRegister :: Registration -> IntrayHandler NoContent
servePostRegister Registration {..} = do
    maybeHashedPassword <- liftIO $ passwordHash registrationPassword
    case maybeHashedPassword of
        Nothing -> throwError err400 {errBody = "Failed to hash password."}
        Just hashedPassword -> do
            uuid <- liftIO nextRandomUUID
            now <- liftIO getCurrentTime
            let user =
                    User
                    { userIdentifier = uuid
                    , userUsername = registrationUsername
                    , userHashedPassword = hashedPassword
                    , userCreatedTimestamp = now
                    , userLastLogin = Nothing
                    }
            maybeUserEntity <-
                runDb . getBy $ UniqueUsername $ userUsername user
            case maybeUserEntity of
                Nothing -> runDb $ insert_ user
                Just _ ->
                    throwError
                        err409
                        { errBody =
                              LB.fromStrict $
                              TE.encodeUtf8 $
                              T.unwords
                                  [ "Account with the username"
                                  , usernameText registrationUsername
                                  , "already exists."
                                  ]
                        }
    pure NoContent
