module Config ( RawConfig (..)
              , Config (..)
              , convertConfig ) where

import System.Directory (getCurrentDirectory)
import Data.Maybe (isJust, fromJust)

data RawConfig = RawConfig { rawRoot :: Maybe String
                           , rawExclusion :: Maybe String
                           , rawCollapsible :: Bool }
    deriving Show

data Config = Config { root :: String
                     , excludeList :: [String]
                     , collapsible :: Bool }
    deriving Show

convertConfig :: RawConfig -> IO Config
convertConfig (RawConfig rawRoot rawExclusion rawCollapsible) = do
    let collapsible = rawCollapsible
    let excludeList | isJust rawExclusion = (splitBy ',' . fromJust) rawExclusion
                    | otherwise = []
    if isJust rawRoot 
        then return (Config (fromJust rawRoot) excludeList collapsible)
        else do
    root <- getCurrentDirectory
    return (Config root excludeList collapsible)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy y [] = []
splitBy y xs = left : splitBy y rest where
    (left, right) = break (== y) xs
    rest = if (null right == False) && (head right == y) then tail right
                                                         else right