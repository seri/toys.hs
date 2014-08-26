module Download (download) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (FilePath, writeFile)
import System.Directory (doesFileExist, getModificationTime)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

isFresh :: FilePath -> IO Bool
isFresh file = do
    exists <- doesFileExist file
    if not exists
        then return False
        else do
    fileTime <- getModificationTime file
    currentTime <- getCurrentTime
    return $ currentTime `diffUTCTime` fileTime <= 3600

download :: String -> FilePath -> IO ()
download url file = do
    fresh <- isFresh file
    if fresh 
        then return ()
        else do
    response <- simpleHTTP . getRequest $ url
    body <- getResponseBody response
    writeFile file body