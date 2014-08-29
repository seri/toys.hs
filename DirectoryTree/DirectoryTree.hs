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
concatMapM f xs = concat `liftM` mapM f xs


data Entry = Entry Int FilePath
instance Show Entry where
    show (Entry level fileName) = times level shiftWidth ++ fileName 

buildEntry :: Int -> FilePath -> FilePath -> IO [Entry]
buildEntry level parent child = do
    let top = Entry level child
    let fullPath = parent </> child
    isDir <- doesDirectoryExist fullPath    
    if isDir
        then buildTree (level + 1) fullPath >>= return . (top :)
        else return [top]

buildTree :: Int -> FilePath -> IO [Entry]
buildTree level parent = do
    allChildren <- getDirectoryContents parent
    let children = filter (`notElem` [".", ".."]) allChildren
    buildEntry level parent `concatMapM` children

showTree :: FilePath -> IO String
showTree root = buildTree 0 root >>= return . unlines . map show


usage :: String
usage = unlines [ 
    "Usage: DirectoryTree <dir>", 
    "Display a tree view of the part of the file system rooted at <dir>" ]

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
        then putStr usage
        else do
    let root = head args
    exists <- doesDirectoryExist root
    if exists
        then showTree root >>= putStr
        else putStr $ "Directory does not exist: " ++ root