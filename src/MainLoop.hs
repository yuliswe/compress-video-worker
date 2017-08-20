module MainLoop where
import           Command
import           Progress
import           Control.Monad.Trans.State
import qualified Control.Monad.Trans.Class  as MT
import Control.Concurrent
import Control.Monad
import Data.HashMap.Lazy as HM
import System.IO as IO

mainLoop :: IO ()
mainLoop = do
    IO.hSetBuffering IO.stdout IO.LineBuffering
    IO.hSetBuffering IO.stderr IO.LineBuffering
    evalStateT loop HM.empty

loop :: StateT Progresses IO ()
loop = forever $ do
    checkCommands
    -- check input command
    checkProgresses
    MT.lift $ threadDelay (1 * 1000000)
    -- print output

onCommandException :: CommandException -> IO ()
onCommandException = print
