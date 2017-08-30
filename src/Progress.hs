{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Progress where

import           Control.Concurrent
import qualified Control.Exception          as E
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

type Standard = String
type ProcessID = (FilePath, Standard)

checkProgresses :: StateT Progresses IO ()
checkProgresses = do
    st <- get
    newst <- MT.lift $ mapM checkProgress st
    put newst
    MT.lift $ do
        errorYellow ("Number of tasks: " ++ show (length st))
        B8.putStrLn $ A.encode $ fmap json $ elems st

checkProgress :: Progress -> IO Progress
checkProgress p
    | (status (json p) == Progress.Error) = return p
    | otherwise = do
        code <- getProcessExitCode $ handle p
        let j = json p
        case code of
            Just ExitSuccess -> do
                return $ p { json = j { percentage = 100, status = Done } }
            Just (ExitFailure k) -> do
                -- err <- IO.hGetContents (stderr p)
                -- errs <- hGetLinesReverse (stderr p)
                -- let e = reverse $ "Exited with code " ++ show k ++ ".\n" ++ errs ++ errors j
                -- errorRed err
                return $ p { json = j { status = Progress.Error } }
            Nothing -> do
                str <- hGetLastLine (stdout p)
                -- errs <- hGetLinesReverse (stderr p)
                -- update errors
                let j' = j { status = Progress.InProgress }
                -- update percentage
                let hm = A.eitherDecode (B8.pack str) :: Either String (HashMap String Float)
                let j'' = if str == "" then j' else
                        case hm of
                            (Left err) -> error err
                            (Right m) -> j' { percentage = m HM.! "percentage" }
                return $ p { json = j'' }

decodeProgress :: B8.ByteString -> ProgressJSON
decodeProgress s =
    case A.eitherDecode s of
        Left err -> Prelude.error err
        Right p  -> p


hGetLastLine :: Handle -> IO String
hGetLastLine = hGetLastLine' ""
    where
        hGetLastLine' sofar hd = do
            ready <- IO.hReady hd
            if ready then do
                l <- hGetLine hd
                hGetLastLine' l hd
            else return sofar

hGetLinesReverse :: Handle -> IO String
hGetLinesReverse = hGetLinesReverse' ""
    where
        hGetLinesReverse' sofar hd = do
            ready <- IO.hReady hd
            if ready then do
                l <- hGetLine hd
                hGetLinesReverse' (l ++ "\n" ++ sofar) hd
            else return sofar


type Progresses = HashMap (FilePath, Standard) Progress

data Progress = Progress {
      stdin  :: Handle
    , stdout :: Handle
    -- , stderr :: Handle
    , handle :: ProcessHandle
    , json   :: ProgressJSON
}

data ProgressJSON = ProgressJSON {
      url        :: FilePath
    , percentage :: Float
    , status     :: Status
    , standard   :: Standard
    -- , errors     :: String
    , command    :: String
} deriving (Generic, FromJSON, ToJSON, Show, Read)

data Status = InQueue | InProgress | Done | Error | UserStopped deriving (Generic, Read, Show, ToJSON, FromJSON, Eq)
