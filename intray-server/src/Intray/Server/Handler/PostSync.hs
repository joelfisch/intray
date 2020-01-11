{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Intray.Server.Handler.PostSync
  ( servePostSync
  ) where

import Import

import qualified Data.Map as M
import Data.Map (Map)
import Data.Mergeless
import qualified Data.Set as S
import Data.Time
import Data.UUID.Typed
import Database.Persist

import Servant hiding (BadPassword, NoSuchUser)
import Servant.Auth.Server as Auth
import Servant.Auth.Server.SetCookieOrphan ()

import Intray.API
import Intray.Data

import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

servePostSync ::
     AuthResult AuthCookie
  -> SyncRequest ItemUUID TypedItem
  -> IntrayHandler (SyncResponse ItemUUID TypedItem)
servePostSync (Authenticated AuthCookie {..}) sr =
  withPermission authCookiePermissions PermitSync $ do
    now <- liftIO getCurrentTime
    let serverSyncProcessorDeleteMany s = do
          runDb $
            deleteWhere
              [IntrayItemUserId ==. authCookieUserUUID, IntrayItemIdentifier <-. S.toList s]
          pure s -- Just assume that everything was deleted.
        serverSyncProcessorQueryNoLongerSynced s = do
          items <-
            runDb $
            selectList
              [IntrayItemUserId ==. authCookieUserUUID, IntrayItemIdentifier <-. S.toList s]
              []
          let inSButNotInStore =
                s `S.difference` S.fromList (map (intrayItemIdentifier . entityVal) items)
          pure inSButNotInStore
        serverSyncProcessorQueryNewRemote s =
          M.fromList . map (makeSynced . entityVal) <$>
          runDb
            (selectList
               [IntrayItemUserId ==. authCookieUserUUID, IntrayItemIdentifier /<-. S.toList s]
               [])
        serverSyncProcessorInsertMany ::
             Map ClientId (Added TypedItem)
          -> IntrayHandler (Map ClientId (ClientAddition ItemUUID))
        serverSyncProcessorInsertMany m = do
          c <- runDb $ count [IntrayItemUserId ==. authCookieUserUUID]
          mss <- asks (fmap monetisationEnvMaxItemsFree . envMonetisation)
          mFunc <-
            case mss of
              Nothing -> pure id
              Just maxItemsFree -> do
                mu <- runDb $ getBy $ UniqueUserIdentifier authCookieUserUUID
                case mu of
                  Nothing -> throwAll err404
                  Just (Entity _ User {..}) -> do
                    isAdmin <- asks ((userUsername `elem`) . envAdmins)
                    pure $
                      if isAdmin
                        then id
                        else take (maxItemsFree - c)
          let m' = mFunc $ M.toList m
          fmap M.fromList $
            forM m' $ \(cid, Added {..}) -> do
              uuid <- nextRandomUUID
              let ii = makeIntrayItem authCookieUserUUID uuid now addedValue
              runDb $ insert_ ii
              let ca = ClientAddition {clientAdditionId = uuid, clientAdditionTime = now}
              pure (cid, ca)
        proc = ServerSyncProcessor {..}
    processServerSyncCustom proc sr
servePostSync _ _ = throwAll err401
