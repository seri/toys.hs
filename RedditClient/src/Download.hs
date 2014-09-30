module Download (download) where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import System.IO (FilePath, writeFile)
import System.Directory (doesFileExist, getModificationTime)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

import Control.Applicative (liftA2)

isFresh :: FilePath -> IO Bool
isFresh file = do
    exists <- doesFileExist file
    if not exists
        then return False
        else (<= 3600) `fmap` 
             liftA2 diffUTCTime getCurrentTime (getModificationTime file)

download :: String -> FilePath -> IO ()
download url file = do
    fresh <- isFresh file
    if fresh 
        then return ()
        else (simpleHTTP . getRequest) url >>= getResponseBody >>= writeFile file