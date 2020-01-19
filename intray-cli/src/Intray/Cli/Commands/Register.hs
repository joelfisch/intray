{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}

module Intray.Cli.Commands.Register
  ( register
  ) where

import Import

import Intray.API

import Intray.Client

import Intray.Cli.Client
import Intray.Cli.OptParse
import Intray.Cli.Prompt

register :: RegisterSettings -> CliM ()
register RegisterSettings {..} = do
  registration <-
    Registration <$> promptUsername registerSetUsername <*> promptPassword registerSetPassword
  void $ runSingleClientOrErr $ clientPostRegister registration
