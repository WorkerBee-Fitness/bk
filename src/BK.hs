module BK 
    () where

import Data.Text (Text)
import Data.Csv(FromRecord, ToRecord)
import GHC.Generics (Generic)

data Bookmark = Bookmark {
    label :: !Text,
    cmd :: !Text
} deriving (Generic, Show, Eq)

instance FromRecord Bookmark
instance ToRecord Bookmark
