{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Intray.Data.Gen where

import Import

import System.IO.Unsafe

import qualified Data.Text as T

import Intray.Data

instance GenUnchecked ItemType

instance GenValid ItemType where
    genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked IntrayItem

instance GenValid IntrayItem where
    genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked Username

instance GenValid Username where
    genValid = do
        username <- parseUsername <$> textGen
        case username of
            Just name -> pure name
            Nothing -> genValid
      where
        textGen =
            T.pack <$>
            ((:) <$> charGen <*>
             ((:) <$> charGen <*> ((:) <$> charGen <*> genListOf charGen)))
        charGen = genValid `suchThat` validUsernameChar

instance GenUnchecked User

instance GenValid User where
    genValid = genValidStructurally

instance GenUnchecked HashedPassword

instance GenValid HashedPassword where
    genValid = do
        t <- genValid
        case unsafePerformIO $ passwordHash t of
            Nothing ->
                error
                    "unable to hash password during generation, for some reason"
            Just hp -> pure hp

instance GenUnchecked Permission

instance GenValid Permission where
    genValid = genValidStructurallyWithoutExtraChecking

instance GenUnchecked AccessKeySecret

instance GenValid AccessKeySecret where
    genValid = genValidStructurally
