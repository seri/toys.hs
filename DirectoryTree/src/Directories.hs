module Directories (showTree) where

import Config (Config(..))

import Data.Maybe (isJust, fromJust)
import Data.Monoid (Monoid, mconcat)
import Control.Monad (liftM)

import System.FilePath ((</>))
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.Environment (getArgs)

times :: Monoid a => Int -> a -> a
times n = mconcat . replicate n  

shiftWidth :: String
shiftWidth = times 4 " "

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat `fmap` mapM f xs

data Entry = Entry Int FilePath
instance Show Entry where
    show (Entry level fileName) = times level shiftWidth ++ fileName

getChildren :: FilePath -> IO [FilePath]
getChildren dir = filter (`notElem` [".", ".."]) `fmap` getDirectoryContents dir

getOnlyChild :: FilePath -> IO (Maybe FilePath)
getOnlyChild path = do
    isDir <- doesDirectoryExist path
    if not isDir
        then return Nothing
        else do
    children <- getChildren path
    if length children == 1
        then return (Just (head children))
        else return Nothing

convertChild :: Bool -> FilePath -> FilePath -> IO FilePath
convertChild False parent child = return child
convertChild True parent child = do
    let fullPath = parent </> child
    onlyChild <- getOnlyChild fullPath
    if isJust onlyChild
        then (child </>) `fmap` convertChild True fullPath (fromJust onlyChild)
        else return child

buildEntry :: Config -> Int -> FilePath -> FilePath -> IO [Entry]
buildEntry config level parent child = do
    let top = Entry level child
    let fullPath = parent </> child
    isDir <- doesDirectoryExist fullPath    
    if isDir
        then (top :) `fmap` buildTree config (level + 1) fullPath
        else return [top]

buildTree :: Config -> Int -> FilePath -> IO [Entry]
buildTree config level parent = do
    allChildren <- getChildren parent
    let children = filter (`notElem` (excludeList config)) allChildren
    mapM (convertChild (collapsible config) parent) children >>=
        concatMapM (buildEntry config level parent)

showTree :: Config -> IO String
showTree config = (unlines . map show) `fmap` buildTree config 0 (root config)