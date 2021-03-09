{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import           Hbb
import           XRelude

main :: IO ()
main = do
  progName <- getProgName
  args     <- getArgs
  mainWithArgs progName args
