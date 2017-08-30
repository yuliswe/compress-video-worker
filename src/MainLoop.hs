module MainLoop where
import           Command
import           Control.Concurrent
import qualified Control.Exception         as E
import           Control.Monad
import qualified Control.Monad.Catch       as MC
import qualified Control.Monad.Trans.Class as MT
import           Control.Monad.Trans.State
import           Data.HashMap.Lazy         as HM
import           Debug
import           Progress
import           System.Exit
import           System.IO                 as IO

mainLoop :: IO ()
mainLoop = do
    IO.hSetBuffering IO.stdin IO.LineBuffering
    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering
    IO.hSetEncoding IO.stdin IO.utf8
    IO.hSetEncoding IO.stdout IO.utf8
    IO.hSetEncoding IO.stderr IO.utf8
    evalStateT loop HM.empty

loop :: StateT Progresses IO ()
loop = forever $ MC.handle onAnyException $ do
    checkCommands
    -- check input command
    checkProgresses
    MT.lift $ threadDelay (1 * 1000000)
    -- print output

-- onRequredExit :: ExitCode -> StateT Progresses IO ()
-- onRequredExit e = do
--     MT.lift $ do
--         errorYellow $ displayException e
--         errorYellow "Shutdown all processes."
--     exitWith e

onAnyException :: MC.SomeException -> StateT Progresses IO ()
onAnyException e = do
    let maybeExit = E.fromException e :: Maybe ExitCode
    let maybeCommand = E.fromException e :: Maybe CommandException
    let pick = (maybeExit, maybeCommand)
    case pick of
        (Just ex, _) -> MT.lift $ exitWith ex
        (_, Just cmd) -> MT.lift $ errorRed $ show cmd
        _ -> do
            MT.lift $ errorRed $ show e
            shutdown
