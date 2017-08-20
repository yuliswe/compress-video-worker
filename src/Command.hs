{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE OverloadedStrings     #-}

module Command where

import           Control.Exception
import           Control.Monad
import qualified Control.Monad.Trans.Class  as MT
import           Control.Monad.Trans.State
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import           GHC.Generics
import           Progress                   as P
import           System.Environment
import           System.Exit
import           System.FilePath
import qualified System.IO                  as IO
import qualified System.Process             as P
import Data.HashMap.Lazy as HM
import Debug

data CommandException = NoSuchCommand {
      given  :: String
    , reason :: String
} | NoSuchFile {
      command :: String
    , url :: FilePath
} deriving (Show)

instance Exception CommandException


type Commands = [Command]

data Standard = Bilibili deriving (Show, Read)

data Command =
        StartTask {
              url      :: FilePath
            , standard :: Standard
        }
    |   StopTask {
            url :: FilePath
        }
    |   Quit
    deriving (Show)

data Input = Input {
      command   :: String
    , arguments :: [String]
} deriving (Show, Generic, FromJSON)

checkCommands :: StateT Progresses IO ()
checkCommands = do
    ready <- MT.lift $ IO.hReady IO.stdin
    when ready $ do
        contents <- MT.lift IO.getLine
        let commands = parseCommands $ B.pack contents
        -- run commands
        runCommands commands

runCommands :: Commands -> StateT Progresses IO ()
runCommands = mapM_ runCommand

runCommand :: Command -> StateT Progresses IO ()
runCommand (StartTask fp std) = startTask fp std
runCommand (StopTask fp) = stopTask fp
runCommand Quit = shutdown

stopTask :: FilePath -> StateT Progresses IO ()
stopTask fp = do
    st <- get
    case HM.lookup fp st of
        Just p -> do
            MT.lift $ shutdownProcess p
            put $ HM.delete fp st
        Nothing -> throw $ NoSuchFile "stopTask" fp

startTask :: FilePath -> Standard -> StateT Progresses IO ()
startTask fp std = do
    st <- get
    if member fp st then
        MT.lift $ errYellow ("Task " ++ fp ++ " is already running.")
    else do
        config <- MT.lift $ locateConfigFile std
        compressVideoBin <- MT.lift $ getEnv "bin_compress_video"
        let ext = takeExtensions fp
        let outUrl = replaceExtensions fp (show std ++ ext)
        let args = unwords [compressVideoBin, fp, outUrl, config]
        (Just pstdin, Just pstdout, Just pstderr, hl) <- MT.lift $
            P.createProcess (P.shell args) {
                  P.std_in  = P.CreatePipe
                , P.std_out = P.CreatePipe
                , P.std_err = P.CreatePipe
            }
        let newProgess = Progress {
            json = ProgressJSON {
                  url = fp
                , percentage = 0
                , status = InQueue
                , errors = ""
            }
            , stdin  = pstdin
            , stdout = pstdout
            , stderr = pstderr
            , handle = hl
        }
        put (insert fp newProgess st)
        MT.lift $ errYellow ("Started task: " ++ args)
        return ()

locateConfigFile :: Standard -> IO FilePath
locateConfigFile std = getEnv ("cfg_" ++ show std)

parseCommands :: B.ByteString -> Commands
parseCommands contents = [ convertToCmd (B.unpack l) $ eitherDecode l | l <- B.split '\n' contents ]
    where
        convertToCmd :: String -> Either String Input -> Command
        convertToCmd _ (Right (Input "startTask" [fp, std])) = StartTask fp (read std)
        convertToCmd _ (Right (Input "stopTask" [fp])) = StopTask fp
        convertToCmd _ (Right (Input "quit" [])) = Quit
        convertToCmd cmd (Left err) = throw $ NoSuchCommand cmd err
        convertToCmd cmd _ = throw $ NoSuchCommand cmd "invalid arguments"

shutdown :: StateT Progresses IO ()
shutdown = do
    ps <- get
    MT.lift $ do
        shutdownProcesses ps
        exitSuccess

shutdownProcesses :: Progresses -> IO ()
shutdownProcesses ps = do
        mapM_ shutdownProcess ps
        errYellow "Graceful shutdown."

shutdownProcess :: Progress -> IO ()
shutdownProcess p = do
    IO.hPutStrLn (stdin p) "{\"command\": \"quit\"}"
    errYellow ("Waiting for " ++ (P.url $ P.json p) ++ " to quit.")
    void $ P.waitForProcess (P.handle p)
