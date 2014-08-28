module Listing (Listing(..), Post(..), fromJson) where

import Data.Maybe (catMaybes)
import Data.Scientific (Scientific)
import Data.List (intercalate)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as M

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as B
import Data.String.Unicode (unicodeRemoveNoneAscii)

import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), addUTCTime)
import qualified Data.Time.Format

import Control.Lens ((^.))
import qualified Data.Aeson as J
import Data.Aeson.Lens (key)


toUTCTime :: Scientific -> UTCTime
toUTCTime seconds = (fromInteger . round) seconds `addUTCTime`
                    UTCTime (fromGregorian 1970 1 1) (fromInteger 0)

toInt :: Scientific -> Int
toInt = fromInteger . round

toString :: T.Text -> String
toString = unicodeRemoveNoneAscii . T.unpack


data Post = Post {
    postTitle :: String ,
    postDomain :: String ,
    postAuthor :: String ,
    postScore :: Int ,
    postCommentCount :: Int ,
    postCreatedUTC :: UTCTime
}

data Listing = Listing [Post]


fromJson :: String -> Maybe Listing
fromJson json = J.decode (B.pack json) >>= 
    flip (^.) (key (T.pack "data") . key (T.pack "children")) >>= 
    toListing . catMaybes . map toPost . V.toList

toListing :: [Post] -> Maybe Listing
toListing [] = Nothing
toListing posts = Just $ Listing posts


toPost :: J.Value -> Maybe Post
toPost (J.Object obj) = M.lookup (T.pack "data") obj >>= toPost'
toPost _ = Nothing

toPost' :: J.Value -> Maybe Post
toPost' (J.Object obj) = toPost'' . catMaybes . map (flip M.lookup obj . T.pack)
    $ [ "title", "domain", "author", "score", "num_comments", "created_utc" ] 
toPost' _ = Nothing

toPost'' :: [J.Value] -> Maybe Post
toPost'' [ J.String title, J.String domain, J.String author
         , J.Number score, J.Number commentCount, J.Number createdUTC ] =
    Just $ Post (toString title) (toString domain) (toString author)
                (toInt score) (toInt commentCount) (toUTCTime createdUTC)
toPost'' _ = Nothing