module Debug where

import           System.Console.ANSI
import qualified System.IO as IO

errorYellow :: String -> IO ()
errorYellow str = do
    hSetSGR IO.stderr [SetColor Foreground Vivid Yellow]
    IO.hPutStrLn IO.stderr str
    hSetSGR IO.stderr [Reset]

errorRed :: String -> IO ()
errorRed str = do
    hSetSGR IO.stderr [SetColor Foreground Vivid Red]
    IO.hPutStrLn IO.stderr str
    hSetSGR IO.stderr [Reset]

hSetRed :: IO.Handle -> IO ()
hSetRed hd = hSetSGR hd [SetColor Foreground Vivid Red]
