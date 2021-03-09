{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Hbb where

import qualified Control.Exception.Safe as CES
import           Control.Monad          ((>=>))
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Relude
import qualified System.Directory       as SD
import           System.Environment     (getArgs, getProgName, setEnv, unsetEnv)
import qualified System.Exit            as SE
import qualified System.IO              as SIO
import qualified System.IO.Error        as SIOE
import qualified System.Process         as SP


yes :: Text -> IO ()
yes = forever . putTextLn

readingFile :: (Text -> IO a) -> FilePath -> IO a
readingFile fun file = withFile file ReadMode $ TIO.hGetContents >=> fun

wcC :: FilePath -> IO ()
wcC = readingFile $ putTextLn . show . T.length

wcL :: FilePath -> IO ()
wcL = readingFile $ putTextLn . show . length . lines

cat :: FilePath -> IO ()
cat = readingFile putText

sh = do
  SIO.hSetBuffering SIO.stderr SIO.NoBuffering
  SIO.hSetBuffering SIO.stdin SIO.NoBuffering
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  putText "> "
  line   <- getLine
  result <- CES.tryAny $ case words line of
    ["exit"] -> exitSuccess
    ["cd"  ] -> SD.getHomeDirectory >>= SD.setCurrentDirectory
    ["cd"   , d              ] -> SD.setCurrentDirectory $ toString d
    ["unset", name           ] -> unsetEnv $ toString name
    ["set"  , nameEqualsValue] -> case T.split ('=' ==) nameEqualsValue of
      [name , value] -> setEnv (toString name) (toString value)
      _              -> exitFailure
    (progName : args) -> do
      let process = SP.proc (toString progName) (toString <$> args)
      eitherExceptionOrExitCode <-
        SP.withCreateProcess process { SP.delegate_ctlc = True }
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
