{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Hbb
import           XRelude

main :: IO ()
main = do
  progName <- getProgName
  args     <- getArgs
  mainWithArgs progName args
