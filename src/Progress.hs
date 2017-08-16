{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Progress where

import qualified Control.Monad.Trans.Class  as MT
import           Control.Monad.Trans.State
import           Data.Aeson                 as A hiding (json)
import qualified Data.ByteString.Lazy.Char8 as B8
import           Data.HashMap.Lazy          as HM
import           Debug
import           GHC.Generics
import           GHC.IO.Handle
import           System.Exit
import qualified System.IO                  as IO
import           System.Process

checkProgresses :: StateT Progresses IO ()
checkProgresses = do
    st <- get
    newst <- MT.lift $ mapM checkProgress st
    put newst
    MT.lift $ do
        errYellow ("Number of tasks: " ++ show (length st))
        B8.putStrLn $ A.encode $ fmap json st

-- printProgress :: Progress -> IO ()
-- printProgress = B8.putStrLn . A.encode

checkProgress :: Progress -> IO Progress
checkProgress p = do
    code <- getProcessExitCode $ handle p
    let j = json p
    case code of
        Just ExitSuccess -> return $ p { json = j { percentage = 100, status = Done } }
        Just _ -> return $ p { json = j { status = Progress.Error } }
        Nothing -> do
            ready <- IO.hReady (stdout p)
            if ready then do
                str <- IO.hGetLine (stdout p)
                return $ p { json = decodeProgress $ B8.pack str }
            else
                return p

            -- cent <- checkPercentage p
            -- return $ p { percentage = cent, status = InProgress }

decodeProgress :: B8.ByteString -> ProgressJSON
decodeProgress s =
    case A.eitherDecode s of
        Left err -> error err
        Right p -> p

-- checkPercentage :: Progress -> IO Float
-- checkPercentage p = do
--     ready <- hReady (stdout p)
--     when ready $ do
--         str <- hGetLine (stdout p)



type Progresses = HashMap FilePath Progress

data Progress = Progress {
      stdin      :: Handle
    , stdout     :: Handle
    , stderr     :: Handle
    , handle     :: ProcessHandle
    , json :: ProgressJSON
}

data ProgressJSON = ProgressJSON {
      url :: FilePath
    , percentage :: Float
    , status :: Status
} deriving (Generic, FromJSON, ToJSON, Show, Read)

data Status = InQueue | InProgress | Done | Error | UserStopped deriving (Generic, Read, Show, ToJSON, FromJSON)
