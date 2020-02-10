{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Intray.Web.Server.Handler.Process
  ( getProcessR
  , postDoneR
  ) where

import Import

import Yesod

import Intray.API
import Intray.Client

import Intray.Web.Server.Foundation
import Intray.Web.Server.Time

getProcessR :: Handler Html
getProcessR =
  withLogin $ \t -> do
    mi <- runClientOrDisallow $ clientGetShowItem t
    case mi of
      Nothing -> withNavBar $(widgetFile "process-unauthorised")
      Just mItem -> do
        mItemWidget <-
          case mItem of
            Nothing -> pure Nothing
            Just i -> Just <$> makeItemInfoWidget i
        nrItems <- runClientOrErr $ length <$> clientGetItemUUIDs t
        withNavBar $(widgetFile "process")

makeItemInfoWidget :: ItemInfo TypedItem -> Handler Widget
makeItemInfoWidget ItemInfo {..} = do
  token <- genToken
  timestampWidget <- makeTimestampWidgetNow itemInfoCreated
  pure $(widgetFile "item")

newtype DoneItem =
  DoneItem
    { doneItemUUID :: ItemUUID
    }

doneItemForm :: FormInput Handler DoneItem
doneItemForm = DoneItem <$> ireq hiddenField "item"

postDoneR :: Handler Html
postDoneR =
  withLogin $ \t -> do
    DoneItem {..} <- runInputPost doneItemForm
    void $ runClientOrErr $ clientDeleteItem t doneItemUUID
    redirect ProcessR
