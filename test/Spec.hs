{-# LANGUAGE OverloadedStrings #-}

import           Command
import           MainLoop

main :: IO ()
main = do
    -- {"command": "startTask", "arguments": ["/Users/ylilarry/Movies/1.1.flv","Bilibili"]}
    -- {"command": "stopTask", "arguments": ["test"]}
    -- {"command": "quit", "arguments": []}
    -- {"arguments":["G:/ylilarry/Videos/\xE6\x9E\x95\xE8\xBE\xB9\xE6\xB8\xB8\xE6\x88\x8F.mp4","AcFun"],"command":"startTask"}
    -- {"arguments":["G:\\ylilarry\\Videos\\绳奴体验吊绑_h264.mp4","AcFun"],"command":"startTask"}
    -- {"arguments":["G:\ylilarry\Videos\\231\187\179\229\165\180\228\189\147\233\170\140\229\144\138\231\187\145_h264.mp4","AcFun"],"command":"startTask"}
    print $ parseCommands "{\"command\": \"startTask\", \"arguments\": [\"test\",\"Bilibili\"]}"
