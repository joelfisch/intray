{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Intray.Server.Handler.PostChangePassphrase
  ( servePostChangePassphrase
  ) where

import Import

import Database.Persist

import Servant

import Intray.API

import Intray.Server.Types

import Intray.Server.Handler.Utils

servePostChangePassphrase :: AuthCookie -> ChangePassphrase -> IntrayHandler NoContent
servePostChangePassphrase AuthCookie {..} ChangePassphrase {..} = do
  mUser <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
  case mUser of
    Nothing -> throwError $ err404 {errBody = "User not found."}
    Just (Entity uid User {..}) ->
      if validatePassword userHashedPassword changePassphraseOld
        then do
          mhp <- liftIO $ passwordHash changePassphraseNew
          case mhp of
            Nothing -> throwError $ err500 {errBody = "Unable to hash new password."}
            Just hp -> do
              runDb $ update uid [UserHashedPassword =. hp]
              pure NoContent
        else throwError $ err403 {errBody = "Old password does not match."}
