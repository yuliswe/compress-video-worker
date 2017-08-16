{-# LANGUAGE OverloadedStrings #-}

import           MainLoop
import Command

main :: IO ()
main = do
    -- {"command": "startTask", "arguments": ["test","Bilibili"]}
    -- {"command": "stopTask", "arguments": ["test"]}
    -- {"command": "quit", "arguments": []}
    print $ parseCommands "{\"command\": \"startTask\", \"arguments\": [\"test\",\"Bilibili\"]}"
