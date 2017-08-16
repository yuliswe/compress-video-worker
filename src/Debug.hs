module Debug where

import           System.Console.ANSI
import qualified System.IO as IO

errYellow :: String -> IO ()
errYellow str = do
    hSetSGR IO.stderr [SetColor Foreground Vivid Yellow]
    IO.hPutStrLn IO.stderr str
    hSetSGR IO.stderr []
