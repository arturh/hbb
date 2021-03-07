{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Control.Exception.Safe as CES
import qualified Data.Text              as T
import qualified Data.Text.IO           as TIO
import           Relude
import qualified System.Directory       as SD
import           System.Environment     (getArgs, getProgName)
import qualified System.Exit            as SE
import qualified System.IO              as SIO
import qualified System.IO.Error        as SIOE
import qualified System.Process         as SP

import           Lib

main :: IO ()
main = do
  progName <- fmap toText getProgName
  args     <- fmap (fmap toText) getArgs
  mainWithArgs progName args

mainWithArgs :: Text -> [Text] -> IO ()
mainWithArgs progName args = case (progName, args) of
  ("cat"    , _                ) -> cat args
  ("echo"   , ("-n" : args')   ) -> putText . unwords $ args'
  ("echo"   , _                ) -> putTextLn . unwords $ args
  ("false"  , _                ) -> exitFailure
  ("sh"     , []               ) -> sh
  ("true"   , _                ) -> exitSuccess
  ("wc"     , ["-c", file]     ) -> wcC file
  ("wc"     , ["-l", file]     ) -> wcL file
  ("yes"    , _                ) -> yes args
  ("hbb-exe", ["-h"]           ) -> printHelp
  ("hbb-exe", ["--help"]       ) -> printHelp
  ("hbb-exe", progName' : args') -> mainWithArgs progName' args'
  _                              -> do
    printHelp
    putTextLn
      $ unlines [unwords ["progName", progName], unwords $ "args" : args]
    exitFailure

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
    --let len = length contents in putTextLn $ toText len
    pure ()

wcL :: Text -> IO ()
wcL file = do
  withFile (toString file) ReadMode $ \h -> do
    contents <- TIO.hGetContents h
    putTextLn $ show . length $ lines contents

cat :: [Text] -> IO ()
cat []       = pure ()
cat (f : fs) = do
  withFile
    (toString f)
    ReadMode
    (\h -> do
      contents <- TIO.hGetContents h
      putText contents
    )
  cat fs


sh = do
  SIO.hSetBuffering SIO.stderr SIO.NoBuffering
  SIO.hSetBuffering SIO.stdin SIO.NoBuffering
  SIO.hSetBuffering SIO.stdout SIO.NoBuffering
  putText "> "
  line <- getLine
  case fmap toString $ words line of
    ["exit"] -> exitSuccess
    ["cd"  ] -> do
      homeDir <- SD.getHomeDirectory
      SD.setCurrentDirectory homeDir
    ["cd", d] -> do
      result <- CES.tryAny $ SD.setCurrentDirectory d
      case result of
        Right () -> pure ()
        Left  e  -> do
          putStrLn $ displayException e
          putStr "err"
    (progName : args) -> do
      eitherExceptionOrExitCode <-
        CES.tryAny
        $ SP.withCreateProcess (SP.proc progName args) { SP.delegate_ctlc = True
                                                       }
        $ \_ _ _ p -> SP.waitForProcess p
      case eitherExceptionOrExitCode of
        Right SE.ExitSuccess     -> return ()
        Right (SE.ExitFailure r) -> putText $ show r
        Left  e                  -> do
          putStrLn $ displayException e
          putText "err"
    [] -> pure ()

  sh
