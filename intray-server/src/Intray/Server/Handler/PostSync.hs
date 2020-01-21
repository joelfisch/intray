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

import Intray.API

import Intray.Server.Handler.Stripe
import Intray.Server.Handler.Utils
import Intray.Server.Item
import Intray.Server.Types

servePostSync ::
     AuthCookie -> SyncRequest ItemUUID TypedItem -> IntrayHandler (SyncResponse ItemUUID TypedItem)
servePostSync AuthCookie {..} sr = do
  ps <- getUserPaidStatus authCookieUserUUID
  doSync ps authCookieUserUUID sr

doSync ::
     PaidStatus
  -> AccountUUID
  -> SyncRequest ItemUUID TypedItem
  -> IntrayHandler (SyncResponse ItemUUID TypedItem)
doSync ps userId sr = do
  now <- liftIO getCurrentTime
  let serverSyncProcessorDeleteMany s = do
        runDb $ deleteWhere [IntrayItemUserId ==. userId, IntrayItemIdentifier <-. S.toList s]
        pure s -- Just assume that everything was deleted.
      serverSyncProcessorQueryNoLongerSynced s = do
        items <-
          runDb $ selectList [IntrayItemUserId ==. userId, IntrayItemIdentifier <-. S.toList s] []
        let inSButNotInStore =
              s `S.difference` S.fromList (map (intrayItemIdentifier . entityVal) items)
        pure inSButNotInStore
      serverSyncProcessorQueryNewRemote s =
        M.fromList . map (makeSynced . entityVal) <$>
        runDb (selectList [IntrayItemUserId ==. userId, IntrayItemIdentifier /<-. S.toList s] [])
      serverSyncProcessorInsertMany ::
           Map ClientId (Added TypedItem) -> IntrayHandler (Map ClientId (ClientAddition ItemUUID))
      serverSyncProcessorInsertMany m = do
        let mFunc =
              case ps of
                HasNotPaid i -> take i
                HasPaid _ -> id
                NoPaymentNecessary -> id
        let m' = mFunc $ M.toList m
        fmap M.fromList $
          forM m' $ \(cid, Added {..}) -> do
            uuid <- nextRandomUUID
            let ii = makeIntrayItem userId uuid addedCreated now addedValue
            runDb $ insert_ ii
            let ca = ClientAddition {clientAdditionId = uuid, clientAdditionTime = now}
            pure (cid, ca)
      proc = ServerSyncProcessor {..}
  processServerSyncCustom proc sr
