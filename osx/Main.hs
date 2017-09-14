module Main where

import           MainLoop
import           System.Environment

main :: IO ()
main = do
    setEnv "cfg_acfun" "G:\\ylilarry\\Desktop\\acfun.h264"
    setEnv "cfg_bilibili" "G:\\ylilarry\\Desktop\\bilibili.h264"
    setEnv "bin_compress_video" "compress-video"
    setEnv "bin_ffmpeg" "G:\\bin\\ffmpeg"
    setEnv "bin_ffprobe" "G:\\bin\\ffprobe"
    mainLoop

