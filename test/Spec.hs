{-# LANGUAGE OverloadedStrings #-}

import           Command
import           MainLoop

main :: IO ()
main = do
    -- {"command": "startTask", "arguments": ["/Users/ylilarry/Movies/1.1.flv","Bilibili"]}
    -- {"command": "stopTask", "arguments": ["test"]}
    -- {"command": "quit", "arguments": []}
    print $ parseCommands "{\"command\": \"startTask\", \"arguments\": [\"test\",\"Bilibili\"]}"
