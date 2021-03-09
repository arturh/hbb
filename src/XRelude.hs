{-# LANGUAGE NoImplicitPrelude    #-}
module XRelude (
  module Relude,
  getArgs
  , getProgName
  , setCurrentDirectory
  , setEnv
  , unsetEnv
  , proc
  ) where
import Relude
import qualified System.Environment as SEn
import qualified System.Directory as SD
import qualified System.Process as SP
import qualified System.IO as SIO

getArgs :: IO [Text]
getArgs = fmap (fmap toText) SEn.getArgs

getProgName :: IO Text
getProgName = fmap toText SEn.getProgName

setEnv :: Text -> Text -> IO ()
setEnv name value = SEn.setEnv (toString name) (toString value)

unsetEnv :: Text -> IO ()
unsetEnv name = SEn.unsetEnv (toString name)

setCurrentDirectory :: Text -> IO ()
setCurrentDirectory d = SD.setCurrentDirectory (toString d)

proc :: Text -> [Text] -> SP.CreateProcess
proc progName args = SP.proc (toString progName) (fmap toString args)

withFile :: Text -> IOMode -> (Handle -> IO a) -> IO a
withFile filename = SIO.withFile (toString filename)
