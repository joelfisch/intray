{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Intray.Web.Server.Handler.Add
  ( getAddR
  , postAddR
  ) where

import Import

import Yesod

import Intray.Client

import Intray.Web.Server.Foundation

getAddR :: Handler Html
getAddR =
  withLogin $ \_ -> do
    token <- genToken
    withNavBar $(widgetFile "add")

newtype NewItem =
  NewItem
    { newItemText :: Textarea
    }

newItemForm :: FormInput Handler NewItem
newItemForm = NewItem <$> ireq textareaField "contents"

postAddR :: Handler Html
postAddR =
  withLogin $ \t -> do
    NewItem {..} <- runInputPost newItemForm
    md <- runClientOrDisallow $ clientPostAddItem t $ textTypedItem $ unTextarea newItemText
    when (isNothing md) $ addNegativeMessage "You are not allowed to add items."
    redirect AddR
