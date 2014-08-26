-- Refer to RFC 5905 for details on the network time protocol:
--      http://tools.ietf.org/html/rfc5905

import Data.Bits (shiftL, (.|.))
import Data.Char (ord, chr)
import qualified Data.ByteString.Char8 as B

import Data.Time.Clock (UTCTime(UTCTime), addUTCTime)
import Data.Time.Calendar (fromGregorian)
import Data.Time.LocalTime (getCurrentTimeZone, utcToZonedTime)

import qualified Network.Socket as Net
import qualified Network.Socket.ByteString as NetB

type AddrInfo = (Net.Family, Net.SockAddr)

getAddrInfo :: Net.HostName -> Net.ServiceName -> IO AddrInfo
getAddrInfo host port = Net.withSocketsDo $ do
    addrInfo <- Net.getAddrInfo Nothing (Just host) (Just port)
    let addr = head addrInfo
    return (Net.addrFamily addr, Net.addrAddress addr)

queryUDP :: AddrInfo -> B.ByteString -> Int -> IO B.ByteString
queryUDP (family, sockAddr) request responseSize = Net.withSocketsDo $ do
    sock <- Net.socket family Net.Datagram Net.defaultProtocol
    Net.connect sock sockAddr
    NetB.sendAll sock request
    response <- NetB.recv sock responseSize
    Net.close sock
    return response
    

-- The first byte in ntpRequest (0x1b) means that:
--      Leap indicator = 0      no warning
--      Version number = 3      IPv4 only
--      Mode = 3                client mode

ntpServer = "pool.ntp.org"
ntpPort = "123"
ntpRequestSize = 48
ntpResponseSize = 128

ntpRequest :: B.ByteString
ntpRequest = B.pack . map chr $ 0x1b : replicate (ntpRequestSize - 1) 0

getNtpResponse :: IO B.ByteString
getNtpResponse = Net.withSocketsDo $ do
    ntpAddrInfo <- getAddrInfo ntpServer ntpPort
    ntpResponse <- queryUDP ntpAddrInfo ntpRequest ntpResponseSize
    return ntpResponse


-- The 32-bit unsigned number constructed by the four bytes starting at byte
-- 40th of an NTP response gives up the number of seconds since the beginning
-- of time, which is obviously January the first, 1900. 

composeWord :: [Int] -> Integer
composeWord = composeWord' . map fromIntegral where
    composeWord' :: [Integer] -> Integer
    composeWord' [x1, x2, x3, x4] = shiftL x1 24 .|.
                                    shiftL x2 16 .|.
                                    shiftL x3 8 .|.
                                    x4

extractNtpSeconds :: B.ByteString -> Integer
extractNtpSeconds = composeWord . map ord . take 4 . drop 40 . B.unpack

secondsToTime :: Integer -> UTCTime
secondsToTime seconds = fromInteger seconds `addUTCTime`
    UTCTime (fromGregorian 1900 1 1) (fromInteger 0)


main :: IO ()
main = do
    ntpResponse <- getNtpResponse
    let utcTime = secondsToTime . extractNtpSeconds $ ntpResponse
    timeZone <- getCurrentTimeZone
    let localTime = utcToZonedTime timeZone utcTime
    print localTime