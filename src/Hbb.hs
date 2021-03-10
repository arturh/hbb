{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}

module Hbb where

import qualified Control.Exception.Safe as CES
import           Control.Monad          ((>=>))
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Relude
import qualified System.Directory       as SD
import           System.Environment     (getEnv, setEnv, unsetEnv)
import qualified System.Exit            as SE
import qualified System.IO              as SIO
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

sh :: IO ()
sh = forever $ do
  SIO.hSetBuffering SIO.stderr SIO.NoBuffering
  SIO.hSetBuffering SIO.stdin  SIO.NoBuffering
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  putText "> "
  CES.catch (getLine >>= handleSucces) handleError
    where
      handleSucces line = case words line of
        ["exit" ] -> exitSuccess
        ["cd"   ] -> SD.getHomeDirectory >>= SD.setCurrentDirectory
        ["cd", d] -> SD.setCurrentDirectory $ toString d
        ["unset", name           ] -> unsetEnv $ toString name
        ["set"  , nameEqualsValue] -> case T.split ('=' ==) nameEqualsValue of
          [name , value] -> setEnv (toString name) (toString value)
          _              -> exitFailure
        ["view"  , name          ] -> (getEnv $ toString name) >>= putStrLn
        (progName : args) -> SP.callProcess (toString progName) (toString <$> args)

      handleError e = case fromException e of
          Just SE.ExitSuccess -> exitSuccess
          _                   -> do
            putStrLn $ displayException e
            putStr "err"
