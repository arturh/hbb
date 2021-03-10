{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           Hbb
import           Relude
import           System.Environment (getArgs, getProgName)


type ProgName = String
type Args = [String]

helpMessage :: Text
helpMessage = "hbb-exe false|true|yes|..."

printHelp :: IO ()
printHelp = putTextLn helpMessage

printError :: ProgName -> Args -> IO ()
printError progName args = do
  putTextLn $ unlines
    [ unwords ["progName", toText progName]
    , unwords $ "args" : (toText <$> args)
    ]
  exitFailure


mainWithArgs :: ProgName -> Args -> IO ()
mainWithArgs progName args = case (progName, args) of
  ("cat"    , _                ) -> forM_ args cat
  ("echo"   , "-n" : args'     ) -> putText $ argsToText args'
  ("echo"   , _                ) -> putTextLn $ argsToText args
  ("false"  , _                ) -> exitFailure
  ("sh"     , []               ) -> sh
  ("true"   , _                ) -> exitSuccess
  ("wc"     , ["-c", file]     ) -> wcC file
  ("wc"     , ["-l", file]     ) -> wcL file
  ("yes"    , []               ) -> yes "y"
  ("yes"    , _                ) -> yes $ argsToText args
  ("hbb-exe", ["-h"]           ) -> printHelp
  ("hbb-exe", ["--help"]       ) -> printHelp
  ("hbb-exe", progName' : args') -> mainWithArgs progName' args'
  _                              -> printHelp >> printError progName args
  where
    argsToText :: Args -> Text
    argsToText = unwords . fmap toText

main :: IO ()
main = do
  progName <- getProgName
  args     <- getArgs
  mainWithArgs progName args
