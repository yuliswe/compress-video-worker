module MainLoop where
import           Command
import           Progress
import           Control.Monad.Trans.State
import qualified Control.Monad.Trans.Class  as MT
import Control.Concurrent
import Control.Monad
import Data.HashMap.Lazy as HM

mainLoop :: IO ()
mainLoop = evalStateT loop HM.empty

loop :: StateT Progresses IO ()
loop = forever $ do
    checkCommands
    -- check input command
    checkProgresses
    MT.lift $ threadDelay (1 * 1000000)
    -- print output

onCommandException :: CommandException -> IO ()
onCommandException = print
