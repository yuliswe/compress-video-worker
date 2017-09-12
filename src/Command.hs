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
import           Data.Aeson                 as A
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy.UTF8  as BU
import           Data.HashMap.Lazy          as HM
-- import qualified Data.Text.Lazy             as T
-- import qualified Data.Text.Lazy.Encoding    as T
-- import qualified Data.Text.Lazy.IO          as T
import           Debug
import           GHC.Generics
import           Progress                   as P
import           System.Environment
import           System.Exit
import           System.FilePath
import qualified System.IO                  as IO
import qualified System.Process             as P

data CommandException = NoSuchCommand {
      given  :: String
    , reason :: String
} | NoSuchTask {
      command  :: String
    , url      :: FilePath
    , standard :: Standard
} | TaskAlreadyExists {
      url      :: FilePath
    , standard :: Standard
}deriving (Show)

instance Exception CommandException


type Commands = [Command]

data Command =
        StartTask {
              url      :: FilePath
            , standard :: Standard
        }
    |   StopTask {
              url      :: FilePath
            , standard :: Standard
        }
    |   AddTask {
              url      :: FilePath
            , standard :: Standard
        }
    |   QueueTask {
              url      :: FilePath
            , standard :: Standard
        }
    |   RemoveTask {
              url      :: FilePath
            , standard :: Standard
        }
    |   StopAllTasks
    |   QueueAddedTasks
    |   StartQueuedTasks
    |   Report
    |   Quit
    deriving (Show)

data Input = Input {
      command   :: String
    , arguments :: [String]
} deriving (Show, Generic, FromJSON, ToJSON)

checkCommands :: StateT Progresses IO ()
checkCommands = do
    ready <- MT.lift $ IO.hReady IO.stdin
    when ready $ do
        contents <- MT.lift (B8.fromStrict <$> B.getLine)
        let commands = parseCommands contents
        -- run commands
        runCommands commands

runCommands :: Commands -> StateT Progresses IO ()
runCommands = mapM_ runCommand

runCommand :: Command -> StateT Progresses IO ()
runCommand (StartTask fp std)  = startTask fp std
runCommand (StopTask fp std)   = stopTask fp std
runCommand (QueueTask fp std)  = queueTask fp std
runCommand (AddTask fp std)    = addTask fp std
runCommand (RemoveTask fp std) = removeTask fp std
runCommand StopAllTasks        = stopAllTasks
runCommand QueueAddedTasks     = queueAddedTasks
runCommand StartQueuedTasks    = startQueuedTasks
runCommand Report              = report
runCommand Quit                = shutdown

report :: StateT Progresses IO ()
report = get >>= MT.lift . B8.putStrLn . A.encode . fmap P.json . elems

queueTask :: FilePath -> Standard -> StateT Progresses IO ()
queueTask fp std = do
    st <- get
    case HM.lookup (fp, std) st of
        Just p -> throw $ TaskAlreadyExists fp std
        Nothing -> do
            let newProgress = Progress {
                json = ProgressJSON {
                    url = fp
                    , standard = std
                    , command = ""
                    , status = Queued
                    , percentage = 0
                },
                handles = Nothing
            }
            put (HM.insert (fp, std) newProgress st)

addTask :: FilePath -> Standard -> StateT Progresses IO ()
addTask fp std = do
    st <- get
    case HM.lookup (fp, std) st of
        Just p -> throw $ TaskAlreadyExists fp std
        Nothing -> do
            let newProgress = Progress {
                json = ProgressJSON {
                    url = fp
                    , standard = std
                    , command = ""
                    , status = Added
                    , percentage = 0
                },
                handles = Nothing
            }
            put (HM.insert (fp, std) newProgress st)

removeTask :: FilePath -> Standard -> StateT Progresses IO ()
removeTask fp std = do
    st <- get
    case HM.lookup (fp, std) st of
        Just p -> do
            MT.lift $ shutdownProcess p
            put $ HM.delete (fp, std) st
        Nothing -> throw $ NoSuchTask "removeTask" fp std

stopTask :: FilePath -> Standard -> StateT Progresses IO ()
stopTask fp std = do
    st <- get
    case HM.lookup (fp, std) st of
        Just p -> do
            MT.lift $ shutdownProcess p
            let newProgress = p {
                P.json = (P.json p) {
                    status = UserStopped
                }
            }
            put $ HM.insert (fp, std) newProgress st
        Nothing -> throw $ NoSuchTask "stopTask" fp std

stopAllTasks :: StateT Progresses IO ()
stopAllTasks = get >>= (mapM_ (uncurry stopTask) . HM.keys)

startQueuedTasks :: StateT Progresses IO ()
startQueuedTasks = get >>= (mapM_ (uncurry startQueuedTask) . HM.keys)

startQueuedTask :: FilePath -> Standard -> StateT Progresses IO ()
startQueuedTask fp std = do
    st <- get
    case HM.lookup (fp, std) st of
        Nothing -> startTask fp std
        Just p -> if (status $ P.json p) == Queued then startTask fp std
                  else return ()

queueAddedTasks :: StateT Progresses IO ()
queueAddedTasks = get >>= put . HM.map queueAddedTask

queueAddedTask :: Progress -> Progress
queueAddedTask p
    | ((status $ P.json p) == Added) = p { P.json = (P.json p) { status = Queued } }
    | otherwise = p

startTask :: FilePath -> Standard -> StateT Progresses IO ()
startTask fp std = do
    st <- get
    case HM.lookup (fp, std) st of
        Nothing -> do
            queueTask fp std
            startTask fp std
        Just p -> do
            if (P.status $ P.json p) == InProgress then
                throw $ TaskAlreadyExists fp std
            else do
                config <- MT.lift $ locateConfigFile std
                compressVideoBin <- MT.lift $ getEnv "bin_compress_video"
                let outdir = takeDirectory fp
                (Just pstdin, Just pstdout, _, hl) <- MT.lift $
                    P.createProcess (P.proc compressVideoBin [fp, outdir, config]) {
                        P.std_in  = P.CreatePipe
                        , P.std_out = P.CreatePipe
                        , P.std_err = P.Inherit
                    }
                let newProgess = Progress {
                    json = ProgressJSON {
                        url = fp
                        , percentage = 0
                        , status = InProgress
                        , standard = std
                        -- , errors = ""
                        , command = unwords [compressVideoBin, fp, outdir, config]
                    },
                    handles = Just ProgressHandles {
                        stdin  = pstdin
                        , stdout = pstdout
                        -- , stderr = pstderr
                        , processHandle = hl
                    }
                }
                MT.lift $ do
                    IO.hSetBuffering pstdin IO.LineBuffering
                    IO.hSetBuffering pstdout IO.LineBuffering
                    IO.hSetEncoding pstdin IO.utf8
                    IO.hSetEncoding pstdout IO.utf8
                    -- IO.hSetEncoding pstderr IO.utf8
                put (insert (fp, std) newProgess st)
                MT.lift $ errorYellow ("Started task: " ++ P.showCommandForUser compressVideoBin [fp, outdir, config, "+RTS", "-xc"])
                return ()

locateConfigFile :: Standard -> IO FilePath
locateConfigFile std = getEnv ("cfg_" ++ std)

parseCommands :: B8.ByteString -> Commands
parseCommands contents = [ convertToCmd (BU.toString contents) $ eitherDecode contents ]
    where
        convertToCmd :: String -> Either String Input -> Command
        convertToCmd _ (Right (Input "startTask" [fp, std])) = StartTask fp std
        convertToCmd _ (Right (Input "addTask" [fp, std])) = AddTask fp std
        convertToCmd _ (Right (Input "stopTask" [fp, std])) = StopTask fp std
        convertToCmd _ (Right (Input "queueTask" [fp, std])) = QueueTask fp std
        convertToCmd _ (Right (Input "removeTask" [fp, std])) = RemoveTask fp std
        convertToCmd _ (Right (Input "stopAllTasks" [])) = StopAllTasks
        convertToCmd _ (Right (Input "startQueuedTasks" [])) = StartQueuedTasks
        convertToCmd _ (Right (Input "queueAddedTasks" [])) = QueueAddedTasks
        convertToCmd _ (Right (Input "report" [])) = Report
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
    errorYellow $ "Shutting down all " ++ (show $ length ps) ++ " processes."
    mapM_ shutdownProcess ps
    errorYellow "Gracefully shut down everything."

shutdownProcess :: Progress -> IO ()
shutdownProcess pr = do
    -- open <- IO.hIsOpen ihd
    let js = P.json pr
    let st = P.status js
    if st == InProgress then do
        let (Just phs) = P.handles pr
        let ihd = stdin phs
        IO.hPutStrLn ihd "quit"
        errorYellow $ "Waiting for " ++ P.url js ++ " to quit."
        void $ P.waitForProcess $ P.processHandle phs
    else do
        errorYellow $ "Skip " ++ P.url js ++ " (" ++ show st ++ ")."
