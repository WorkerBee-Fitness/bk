{-# LANGUAGE    LambdaCase        #-}
{-# LANGUAGE    BangPatterns      #-}
{-# LANGUAGE    LinearTypes       #-}
{-# LANGUAGE    NoImplicitPrelude #-}
{-# LANGUAGE    QualifiedDo       #-}
{-# LANGUAGE    OverloadedStrings #-}
{-# LANGUAGE    ViewPatterns      #-}
{-# OPTIONS_GHC -Wno-orphans      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module BK 
    (Bookmark(..),
     BKType(..),  
     parseBKType,   
     readCSVFile,
     writeCSVFile,
     addBookmark,
     removeBookmark,
     findBookmark,
     handler,
     handler_,
     recentBookmarks,
     isAlias,
     isBookmark,
     showBKMap,
     maxOffsetBKMap,
     showBKType,
     filterBKMap) where

-- External Imports:
import Prelude.Linear           (Show(..),
                                 Bool(..),
                                 String,
                                 IO,
                                 Either,
                                 FilePath,
                                 putStrLn,
                                 otherwise)
import Prelude                  (Either (..),
                                 Foldable (..),
                                 Eq (..),
                                 Maybe (..),
                                 Int,
                                 Ord (..),
                                 Num (..),
                                 MonadFail (fail),
                                 ($),
                                 (.),
                                 id,
                                 error,
                                 not,
                                 (<=),
                                 (&&),
                                 map,
                                 (<>),
                                 print, 
                                 undefined)
import GHC.Generics             (Generic)
import Data.Text                (Text, 
                                 concat, 
                                 pack)
import Data.Csv                 (FromRecord(..), 
                                 ToRecord(..), 
                                 FromField (..), 
                                 Field, 
                                 ToField (..),                              
                                 ToNamedRecord (..),                              
                                 NamedRecord, 
                                 Name, 
                                 Parser,
                                 namedRecord, 
                                 (.=),
                                 encodeByName)
import System.Exit              (exitFailure, 
                                 exitSuccess)
import System.FilePath          (isValid,
                                 isAbsolute,
                                 normalise,
                                 dropTrailingPathSeparator)
import Control.Monad            (Monad(..), 
                                 return, 
                                 when)
import GHC.IO.IOMode            (IOMode(..))
import Data.Bifunctor           (Bifunctor(bimap))
import Data.Time.Calendar       (Day, 
                                 addDays)
import Data.Time.Format.ISO8601 (ISO8601 (iso8601Format), 
                                 Format (formatShowM),
                                 formatParseM)

import qualified Control.Functor.Linear    as Linear
import qualified System.IO.Resource.Linear as Linear
import qualified Data.Unrestricted.Linear  as Linear
import qualified Data.Text                 as DT 
import qualified Data.ByteString           as BS
import qualified Data.Csv.Incremental      as CsvInc
import qualified Data.Vector               as Vec
import qualified Data.Map                  as Map

import qualified Lib as Lib

undefined :: a
undefined = Prelude.undefined

data BKType = BKAlias 
            | BKBookmark
    deriving (Generic)

showBKType :: BKType -> Text
showBKType BKAlias    = "alias"
showBKType BKBookmark = "bookmark"

-- | The number of spaces needed to properly space `BKType`'s when pretty
-- printing bookmarks.
--
-- This is equivalent to `length (showBKType bktype) + 1`.
offsetBKType :: BKType -> Int
offsetBKType BKAlias    = 4
offsetBKType BKBookmark = 1

instance Show BKType where
    show :: BKType -> String
    show BKAlias = "alias"
    show BKBookmark = "bookmark"

instance FromField  BKType where
    parseField :: Field -> Parser BKType
    parseField = parseField' . (bimap DT.unpack id) . parseBKType . Lib.byteStringToTextUTF8
        where
            parseField' (Right bt) = return bt
            parseField' (Left err) = fail err

parseBKType 
    :: Text
    -> Either Text BKType
parseBKType s 
        | s == "alias"    = return BKAlias
        | s == "bookmark" = return BKBookmark
        | otherwise       = Left $ s <> " is not a valid bookmark type"

instance ToField BKType where
  toField :: BKType -> Field
  toField BKAlias = "alias"
  toField BKBookmark = "bookmark"

instance FromField Day where
    parseField :: Field -> Parser Day
    parseField s = 
        case formatParseM iso8601Format (Lib.byteStringToStringUTF8 s) of
            Nothing -> _fail $ s <> " is not a valid ISO8601 date"
            Just day -> return day
        where
            _fail = fail . Lib.byteStringToStringUTF8

instance ToField Day where
    toField :: Day -> Field
    toField day = 
        case formatShowM iso8601Format day of
            Nothing -> error "[toField]: failed to pretty print day"
            Just s -> Lib.stringUTF8ToByteString s

data Bookmark = Bookmark {
    bkType     :: !BKType,
    bkLabel    :: !Text,    
    bkTarget   :: !Text,
    bkCreated  :: !Day,
    bkLastUsed :: !Day
} deriving (Generic)

instance Show Bookmark where
    show :: Bookmark -> String  
    show (Bookmark bkT bkL bkTar _ _) 
        = (show bkT) <> " " <> (DT.unpack bkL) <> "=" <> (show bkTar) 

instance FromRecord Bookmark
instance ToRecord   Bookmark

updateBookmarkType :: Bookmark -> BKType -> Bookmark
updateBookmarkType (Bookmark _ bklabel bktarget bkcreated bklastused) bktype
    = Bookmark { 
        bkType     = bktype,          
        bkLabel    = bklabel, 
        bkTarget   = bktarget, 
        bkCreated  = bkcreated, 
        bkLastUsed = bklastused 
    }

updateBookmarkLabel :: Bookmark -> Text -> Bookmark
updateBookmarkLabel (Bookmark bktype _ bktarget bkcreated bklastused) bklabel
    = Bookmark { 
        bkType     = bktype,          
        bkLabel    = bklabel, 
        bkTarget   = bktarget, 
        bkCreated  = bkcreated, 
        bkLastUsed = bklastused 
    }

updateBookmarkTarget :: Bookmark -> Text -> Bookmark
updateBookmarkTarget (Bookmark bktype bklabel _ bkcreated bklastused) bktarget
    = Bookmark { 
        bkType     = bktype,          
        bkLabel    = bklabel, 
        bkTarget   = bktarget, 
        bkCreated  = bkcreated, 
        bkLastUsed = bklastused 
    }

updateBookmarkCreated :: Bookmark -> Day -> Bookmark
updateBookmarkCreated (Bookmark bktype bklabel bktarget _ bklastused) bkcreated
    = Bookmark { 
        bkType     = bktype,          
        bkLabel    = bklabel, 
        bkTarget   = bktarget, 
        bkCreated  = bkcreated, 
        bkLastUsed = bklastused 
    }

updateBookmarkLastUsed :: Bookmark -> Day -> Bookmark
updateBookmarkLastUsed (Bookmark bktype bklabel bktarget bkcreated _) bklastused
    = Bookmark { 
        bkType     = bktype,          
        bkLabel    = bklabel, 
        bkTarget   = bktarget, 
        bkCreated  = bkcreated, 
        bkLastUsed = bklastused 
    }

header :: Vec.Vector Name
header = Vec.fromList 
    ["type",
     "label",
     "target",
     "created-data",
     "last-used"]

instance ToNamedRecord Bookmark where
    toNamedRecord :: Bookmark -> NamedRecord
    toNamedRecord b = namedRecord [
        "type" .= bkType b, 
        "label" .= bkLabel b, 
        "target" .= bkTarget b,
        "created-data" .= bkCreated b,
        "last-used" .= bkLastUsed b]

data BKMap = BKMap {
    getBKMap :: Map.Map Text Bookmark,
    getBKMapMaxLabelLength :: Int
}

emptyBKMap :: BKMap 
emptyBKMap = BKMap { 
        getBKMap = Map.empty, 
        getBKMapMaxLabelLength = 0 
    }

bookmarks :: BKMap -> [Bookmark]
bookmarks = Map.elems . getBKMap

insertBK :: Bookmark -> BKMap -> BKMap
insertBK bk bkMap = BKMap { 
        getBKMap = Map.insert newLabel bk $ getBKMap bkMap, 
        getBKMapMaxLabelLength = (max (maxOffsetBKMap bkMap) (DT.length newLabel)) 
    }
    where
        newLabel = bkLabel bk

maxLabels :: Map.Map Text Bookmark -> Int
maxLabels = Prelude.maximum . Prelude.map DT.length . Map.keys

deleteBookmark :: Text -> BKMap -> BKMap
deleteBookmark label bkMap = BKMap {
        getBKMapMaxLabelLength = newMax,
        getBKMap = newMap
    }
    where    
        newMap = Map.delete label $ getBKMap bkMap
        newMax = Prelude.maximum . Prelude.map DT.length . Map.keys $ newMap

lookupBookmark :: Text -> BKMap -> Prelude.Maybe Bookmark
lookupBookmark label = Map.lookup label . getBKMap

foldlBKMap :: (a -> Bookmark -> a)
           -> a
           -> BKMap
           -> a
foldlBKMap f s = Map.foldl f s . getBKMap

filterBKMap :: (Bookmark -> Bool)
            -> BKMap
            -> BKMap
filterBKMap pred bkMap = BKMap {
        getBKMap = newMap,
        getBKMapMaxLabelLength = newMax
    }
    where
        _map = getBKMap bkMap
        newMap = Map.filter pred _map
        newMax = maxLabels newMap

toAssocList :: BKMap -> [(Text,Bookmark)]
toAssocList (BKMap _map _) = Map.assocs _map

maxOffsetBKMap :: BKMap -> Int
maxOffsetBKMap = getBKMapMaxLabelLength 

labelOffset :: Int -> Text -> Int
labelOffset maxOffset label = maxOffset - (DT.length label)

toBKAssignment :: Int -> BKType -> Text -> Text -> Text
toBKAssignment maxOffset bktype label target = 
       (showBKType bktype) <> DT.replicate (offsetBKType bktype) " "
    <> label <> DT.replicate (labelOffset maxOffset label) " " <> " = " <> (DT.show target)

showBKMap :: Int -> BKMap -> Text
showBKMap maxOffset bkMap = _showBKMap $ toAssocList bkMap
    where
        _showBKMap :: [(Text,Bookmark)] -> Text
        _showBKMap [] = ""
        _showBKMap [(label,bk)] 
             = toBKAssignment maxOffset (bkType bk) label (bkTarget bk)
        _showBKMap ((label,bk):bks) 
             = toBKAssignment maxOffset (bkType bk) label (bkTarget bk) <> "\n"
            <> _showBKMap bks

_logDebug 
    :: String 
    -> Linear.RIO ()
_logDebug !s = Linear.do
    h <- Linear.unsafeAcquire (Linear.return (Linear.Ur ())) (\_ -> Linear.return ())
    r <- Linear.unsafeFromSystemIOResource_ (\_ -> putStrLn s) h
    Linear.release r

feedCSVFile 
    :: (BS.ByteString -> CsvInc.Parser Bookmark)
    -> Linear.Handle %1
    -> Linear.RIO (Linear.Ur (CsvInc.Parser Bookmark), Linear.Handle)
feedCSVFile parserFam csvFile = Linear.do
    (Linear.Ur isEOF, csvFile') <- Linear.hIsEOF csvFile
    if isEOF
    then Linear.return (Linear.Ur (parserFam BS.empty),csvFile')
    else Linear.do
        (Linear.Ur line,csvFile'') <- Linear.hGetLine csvFile'
        let line' = Lib.textUTF8ToByteString (Data.Text.concat [line,(Data.Text.pack "\n")])
        let parser = parserFam line'
        Linear.return (Linear.Ur parser,csvFile'')

updateAcc :: ([Text], BKMap)
          -> [Either String Bookmark]
          -> ([Text], BKMap)
updateAcc acc = Prelude.foldl upAcc acc
    where
        upAcc (errs,bkMap) (Left errMsg) = (errs <> [DT.pack errMsg],bkMap)
        upAcc (errs,bkMap) (Right b)     = (errs,insertBK b bkMap)

loop :: ([Text], BKMap)
     -> Linear.Handle %1
     -> CsvInc.Parser Bookmark 
     -> Linear.RIO (Linear.Ur ([Text], BKMap))
loop acc csvFile (CsvInc.Fail _ errMsg)   
    = Linear.do 
        Linear.release csvFile 
        Linear.return (Linear.Ur (updateAcc acc [Left errMsg]))

loop acc csvFile (CsvInc.Many rs parserFam) 
    = Linear.do         
        (Linear.Ur parser,csvFile'') <- feedCSVFile parserFam csvFile
        loop (updateAcc acc rs) csvFile'' parser                                       

loop acc csvFile (CsvInc.Done rs) 
    = Linear.do     
        Linear.release csvFile 
        Linear.return (Linear.Ur (updateAcc acc rs))

readCSVFile 
    :: FilePath 
    -> IO ([Text], BKMap)
readCSVFile csvFilePath = Linear.run $ readCSVFile' 
    where
        readCSVFile' 
            :: Linear.RIO (Linear.Ur ([Text], BKMap))
        readCSVFile' = Linear.do            
            csvFile <- Linear.openFile csvFilePath ReadMode
            (Linear.Ur contents) <- loop ([],emptyBKMap) csvFile (CsvInc.decode CsvInc.HasHeader) 
            Linear.return (Linear.Ur contents)

writeCSVFile
    :: FilePath
    -> BKMap
    -> IO ()
writeCSVFile csvFilePath bkMap = Linear.run writeCSVFile' 
    where        
        bks = Lib.byteStringToTextUTF8 . 
                Lib.lazyByteStringToByteString $ 
                    encodeByName header $ bookmarks bkMap

        writeCSVFile' :: Linear.RIO (Linear.Ur ())
        writeCSVFile' = Linear.do
            csvFile <- Linear.openFile csvFilePath WriteMode
            csvFile' <- Linear.hPutStr csvFile bks
            Linear.release csvFile'
            Linear.return (Linear.Ur ())

bookmarkAsAssignment :: Bookmark -> Text
bookmarkAsAssignment b = toBKAssignment 0 (bkType b) (bkLabel b) (bkTarget b)

addBookmark :: Bookmark
            -> FilePath
            -> BKMap
            -> Either Text BKMap
addBookmark b homedir bkMap 
    | isBookmark b = do
        let path = dropTrailingPathSeparator . normalise $ DT.unpack (bkTarget b)
        if isValid path
        then  if isAbsolute path
              then return $ insertBK (updateBookmarkTarget b (DT.pack path)) bkMap
              else return $ insertBK (updateBookmarkTarget b (DT.pack . Lib.expandHomeDirectory homedir $ path)) bkMap
        else Left $ "bookmark is not a valid path " <> bookmarkAsAssignment b
    | isAlias b = Right $ insertBK b bkMap
    | otherwise = Left $ "invalid bookmark " <> bookmarkAsAssignment b

removeBookmark :: Text
               -> BKMap
               -> BKMap
removeBookmark = deleteBookmark 

findBookmark :: Text
             -> BKMap
             -> Prelude.Maybe Bookmark
findBookmark = lookupBookmark

partitionBKMap
    :: (Bookmark -> Bool)
    -> BKMap
    -> (BKMap,BKMap)
partitionBKMap pick = foldlBKMap which (emptyBKMap,emptyBKMap)
    where
        which :: (BKMap,BKMap) 
              -> Bookmark 
              -> (BKMap,BKMap)
        which (p1,p2) bk | pick bk   = (insertBK bk p1,p2)
        which (p1,p2) bk | otherwise = (p1,insertBK bk p2)

isAlias :: Bookmark -> Bool
isAlias (Bookmark BKAlias _ _ _ _) = True
isAlias _ = False

isBookmark :: Bookmark -> Bool
isBookmark (Bookmark BKBookmark _ _ _ _) = True
isBookmark _ = False

recentBookmarks :: Day
                -> BKMap
                -> (BKMap,BKMap)
recentBookmarks today bkMap = partitionBKMap isAlias $ filterBKMap recentBookmark bkMap
    where                        
        recentBookmark :: Bookmark -> Bool
        recentBookmark (Bookmark _ _ _ _ lastUsedDay) = 
            addDays (-10) today <= lastUsedDay && lastUsedDay <= today

handler_ :: (BKMap  -> IO ()) -> IO ()
handler_ handle = _handler False (\m -> handle m >> return m)

handler :: (BKMap  -> IO BKMap) -> IO ()
handler = _handler True

initializeWorkDir :: IO FilePath
initializeWorkDir = do 
    homeDir <- Lib.getHomeDirectory
    let wdir = homeDir <> "/.bk"
    let bookmarkCSVFile = wdir <> "/bk-bookmarks.csv"
    Lib.createDirectoryIfMissing False wdir
    bookmarkCSVFileExists <- Lib.doesFileExist bookmarkCSVFile
    when (not bookmarkCSVFileExists) $ 
        writeCSVFile bookmarkCSVFile emptyBKMap
    return bookmarkCSVFile

-- Change (BKMap -> IO BKMap) to ((Int,BKMap) -> IO BKMap).
_handler :: Bool -> (BKMap  -> IO BKMap) -> IO ()
_handler writeMode action = 
    do bookmarkCSVFile <- initializeWorkDir     
       (errs,csvContents) <- readCSVFile bookmarkCSVFile       
       if null errs
       then do csvContents' <- action csvContents                                       
               when writeMode $ writeCSVFile bookmarkCSVFile csvContents'
               exitSuccess
       else print errs >> exitFailure
