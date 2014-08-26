module Main where

import StringUtil (pluralize)
import RelativeTime (showTime)
import Download (download)
import Listing (Listing(..), Post(..), fromJson)

import Data.List (intercalate)
import Data.Maybe (fromJust)

import Control.Applicative (liftA)
import System.IO (readFile)

jsonUrl = "http://www.reddit.com/r/haskell/hot.json" 
jsonFile = "build/hot.json"

showPost :: Post -> IO String
showPost (Post title domain author score commentCount createdUTC) = do
    friendlyTime <- showTime createdUTC
    return $ title ++ " (" ++ domain ++ ")\n    " ++
            pluralize score "point" ++ " by " ++ author ++ " " ++
            friendlyTime ++ " | " ++ pluralize commentCount "comment"

showListing :: Listing -> IO String
showListing (Listing posts) = intercalate "\n" `liftA` mapM showPost posts

main :: IO ()
main = download jsonUrl jsonFile >>
    readFile jsonFile >>= showListing . fromJust . fromJson >>= putStr