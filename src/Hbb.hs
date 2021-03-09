{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Hbb where

import qualified Control.Exception.Safe as CES
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           XRelude
import qualified System.Directory       as SD
import qualified System.Exit            as SE
import qualified System.IO              as SIO
import qualified System.IO.Error        as SIOE
import qualified System.Process         as SP

default (Text)

mainWithArgs :: Text -> [Text] -> IO ()
mainWithArgs progName args = case (progName, args) of
  ("cat"    , _              ) -> cat args
  ("echo"   , "-n":args'     ) -> putText . unwords $ args'
  ("echo"   , _              ) -> putTextLn . unwords $ args
  ("false"  , _              ) -> exitFailure
  ("sh"     , []             ) -> sh
  ("true"   , _              ) -> exitSuccess
  ("wc"     , ["-c", file]   ) -> wcC file
  ("wc"     , ["-l", file]   ) -> wcL file
  ("yes"    , _              ) -> yes args
  ("hbb-exe", ["-h"]         ) -> printHelp
  ("hbb-exe", ["--help"]     ) -> printHelp
  ("hbb-exe", progName':args') -> mainWithArgs progName' args'
  _                            -> do
    printHelp
    putTextLn
      $ unlines [unwords ["progName", progName], unwords $ "args" : args]
    exitFailure
 where
  printHelp :: IO ()
  printHelp = putTextLn "hbb-exe false|true|yes|..."

yes :: [Text] -> IO ()
yes args =
  let theString = case args of
        [] -> "y"
        _  -> unwords args
  in  forever $ putTextLn theString

wcC :: Text -> IO ()
wcC file = do
  withFile (toString file) ReadMode $ \h -> do
    contents <- TIO.hGetContents h
    putTextLn $ show $ T.length contents

wcL :: Text -> IO ()
wcL file = do
  withFile (toString file) ReadMode $ \h -> do
    contents <- TIO.hGetContents h
    putTextLn $ show . length $ lines contents

cat :: [Text] -> IO ()
cat files = forM_ files $ \f -> do
  withFile
    (toString f)
    ReadMode
    ( \h -> do
      contents <- TIO.hGetContents h
      putText contents
    )

sh :: IO ()
sh = do
  SIO.hSetBuffering SIO.stderr SIO.NoBuffering
  SIO.hSetBuffering SIO.stdin  SIO.NoBuffering
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  putText "> "
  line   <- getLine
  result <- CES.tryAny $ case T.words line of
    ["exit"] -> exitSuccess
    ["cd"  ] -> do
      homeDir <- SD.getHomeDirectory
      SD.setCurrentDirectory homeDir
    ["cd"   , d              ] -> setCurrentDirectory d
    ["unset", name           ] -> unsetEnv name
    ["set"  , nameEqualsValue] -> do
      setEnv name value
      where [name, value] = T.split (== '=') nameEqualsValue
    (progName:args) -> do
      eitherExceptionOrExitCode <-
        SP.withCreateProcess (proc progName args) { SP.delegate_ctlc = True }
          $ \_ _ _ p -> SP.waitForProcess p
      case eitherExceptionOrExitCode of
        SE.ExitSuccess   -> pass
        SE.ExitFailure r -> putText $ show r
    [] -> pass

  case result of
    Right () -> pass
    Left  e  -> case fromException e of
      Just SE.ExitSuccess -> exitSuccess
      _                   -> do
        putStrLn $ displayException e
        putStr "err"
  sh
