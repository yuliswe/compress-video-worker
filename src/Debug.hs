module Debug where

import Rainbow
import System.IO

errorYellow :: String -> IO ()
errorYellow str = do
    hPutStrLn stderr str
    

errorRed :: String -> IO ()
errorRed str = do
    hPutStrLn stderr str
