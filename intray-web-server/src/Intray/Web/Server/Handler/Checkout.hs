{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Web.Server.Handler.Checkout
  ( getCheckoutSuccessR
  , getCheckoutCanceledR
  ) where

import Yesod


import Intray.Web.Server.Foundation

getCheckoutSuccessR :: Handler Html
getCheckoutSuccessR = redirect AccountR
getCheckoutCanceledR :: Handler Html
getCheckoutCanceledR = redirect AccountR
