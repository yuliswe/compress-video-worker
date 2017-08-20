module Main where

import           MainLoop
import System.Environment

main :: IO ()
main = do
    setEnv "cfg_Bilibili" "/Users/ylilarry/lab/haskell/compress-video/test/tmp/测试.h264"
    setEnv "bin_compress_video" "compress-video"
    setEnv "bin_ffmpeg" "ffmpeg"
    setEnv "bin_ffprobe" "ffprobe"
    mainLoop

