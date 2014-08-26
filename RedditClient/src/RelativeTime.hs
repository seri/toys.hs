module RelativeTime (showTime) where

import StringUtil (pluralize)
import Data.Time.Clock (UTCTime(..), getCurrentTime, diffUTCTime)

minuteLen = 60
hourLen = minuteLen * 60
dayLen = hourLen * 24

showDiff :: Int -> String
showDiff x = showDiff' x ++ " ago" where
    showDiff' x
        | x >= dayLen = pluralize (div x dayLen) "day"
        | x >= hourLen = pluralize (div x hourLen) "hour"
        | x >= minuteLen = pluralize (div x minuteLen) "minute"
        | otherwise = pluralize x "second"

showTime :: UTCTime -> IO String
showTime utcTime = do
    currentTime <- getCurrentTime
    let diff = diffUTCTime currentTime utcTime
    return . showDiff . round $ diff