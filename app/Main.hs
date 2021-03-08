{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Lib
import           Relude
import           System.Environment (getArgs, getProgName, setEnv, unsetEnv)

main :: IO ()
main = do
  progName <- fmap toText getProgName
  args     <- fmap (fmap toText) getArgs
  mainWithArgs progName args
