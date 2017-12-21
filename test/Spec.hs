{-# LANGUAGE OverloadedStrings #-}

import           Command
import           MainLoop

main :: IO ()
main = do
    print $ parseCommands "{\"command\": \"startTask\", \"arguments\": [\"test\",\"Bilibili\"]}"
