{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
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
     recentBookmarks) where

import Prelude.Linear
    ( Show,
      Bool(False, True),
      String,
      IO,
      Either,
      putStrLn,
      FilePath, otherwise )

import Prelude
    (($),
     (.), Either (..), Foldable (..), Eq (..), Maybe (..), id, error, not, (<=), (&&))

import qualified Control.Functor.Linear as Linear
import qualified System.IO.Resource.Linear as Linear
import qualified Data.Unrestricted.Linear as Linear

import Data.Text (Text, concat, pack)
import qualified Data.Text as DT 

import Data.ByteString as BS hiding (null)
import GHC.Generics (Generic)
import Data.Csv.Incremental (decode, HasHeader (HasHeader))
import qualified Data.Csv.Incremental as CsvInc
import Data.Csv(FromRecord(..), ToRecord(..), FromField (..), Field, ToField (..), encodeByName, ToNamedRecord (..), namedRecord, (.=), NamedRecord, Name, Parser)
import qualified Data.Vector as Vec

import qualified WBeeLib.ByteString as WBL
import qualified WBeeLib.Text as WBL
import qualified WBeeLib.FileSystem as WBL

import System.Exit (exitFailure, exitSuccess)
import Control.Monad (Monad(..), return, when)
import Prelude (print, MonadFail (fail))
import GHC.IO.IOMode (IOMode(..))
import qualified Data.Map as Map
import Prelude ((<>))
import Data.Bifunctor (Bifunctor(bimap))

import Data.Time.Calendar (Day, addDays)
import Data.Time.Format.ISO8601 (formatParseM, ISO8601 (iso8601Format), Format (formatShowM))

data BKType = BKAlias 
            | BKBookmark
    deriving (Generic, Show)

instance FromField  BKType where
    parseField :: Field -> Parser BKType
    parseField = parseField' . (bimap DT.unpack id) . parseBKType . WBL.byteStringToTextUTF8
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
        case formatParseM iso8601Format (WBL.byteStringToStringUTF8 s) of
            Nothing -> _fail $ s <> " is not a valid ISO8601 date"
            Just day -> return day
        where
            _fail = fail . WBL.byteStringToStringUTF8

instance ToField Day where
    toField :: Day -> Field
    toField day = 
        case formatShowM iso8601Format day of
            Nothing -> error "[toField]: failed to pretty print day"
            Just s -> WBL.stringUTF8ToByteString s

data Bookmark = Bookmark {
    bkType     :: !BKType,
    bkLabel    :: !Text,    
    bkTarget   :: !Text,
    bkCreated  :: !Day,
    bkLastUsed :: !Day
} deriving (Generic, Show)

instance FromRecord Bookmark
instance ToRecord   Bookmark

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
        let line' = WBL.textUTF8ToByteString (Data.Text.concat [line,(Data.Text.pack "\n")])
        let parser = parserFam line'
        Linear.return (Linear.Ur parser,csvFile'')

updateAcc :: ([Text], Map.Map Text Bookmark)
          -> [Either String Bookmark]
          -> ([Text], Map.Map Text Bookmark)
updateAcc acc = Prelude.foldl upAcc acc
    where
        upAcc (errs,bkMap) (Left errMsg) = (errs <> [DT.pack errMsg],bkMap)
        upAcc (errs,bkMap) (Right b)     = (errs,Map.insert (bkLabel b) b bkMap)

loop :: ([Text], Map.Map Text Bookmark)
     -> Linear.Handle %1
     -> CsvInc.Parser Bookmark 
     -> Linear.RIO (Linear.Ur ([Text], Map.Map Text Bookmark))
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
    -> IO ([Text], Map.Map Text Bookmark)
readCSVFile csvFilePath = Linear.run $ readCSVFile' 
    where
        readCSVFile' 
            :: Linear.RIO (Linear.Ur ([Text], Map.Map Text Bookmark))
        readCSVFile' = Linear.do            
            csvFile <- Linear.openFile csvFilePath ReadMode
            (Linear.Ur contents) <- loop ([],Map.empty) csvFile (decode HasHeader) 
            Linear.return (Linear.Ur contents)

writeCSVFile
    :: FilePath
    -> Map.Map Text Bookmark
    -> IO ()
writeCSVFile csvFilePath bkMap = Linear.run writeCSVFile' 
    where        
        bks = WBL.byteStringToTextUTF8 . 
                WBL.lazyByteStringToByteString $ 
                    encodeByName header $ Map.elems bkMap

        writeCSVFile' :: Linear.RIO (Linear.Ur ())
        writeCSVFile' = Linear.do
            csvFile <- Linear.openFile csvFilePath WriteMode
            csvFile' <- Linear.hPutStr csvFile bks
            Linear.release csvFile'
            Linear.return (Linear.Ur ())

addBookmark :: Bookmark
            -> Map.Map Text Bookmark
            -> Map.Map Text Bookmark
addBookmark b bkMap = Map.insert (bkLabel b) b bkMap

removeBookmark :: Text
               -> Map.Map Text Bookmark
               -> Map.Map Text Bookmark
removeBookmark label = Map.delete label

findBookmark :: Text
             -> Map.Map Text Bookmark
             -> Prelude.Maybe Bookmark
findBookmark label = Map.lookup label

recentBookmarks :: Day
                -> Map.Map Text Bookmark
                -> Map.Map Text Bookmark
recentBookmarks today = Map.filter recentBookmark 
    where
        recentBookmark :: Bookmark -> Bool
        recentBookmark (Bookmark _ _ _ _ lastUsedDay) = 
            addDays (-10) today <= lastUsedDay && lastUsedDay <= today

handler_ :: (Map.Map Text Bookmark  -> IO ()) -> IO ()
handler_ handle = _handler False (\m -> handle m >> return m)

handler :: (Map.Map Text Bookmark  -> IO (Map.Map Text Bookmark)) -> IO ()
handler = _handler True

initializeWorkDir :: IO FilePath
initializeWorkDir = do 
    homeDir <- WBL.getHomeDirectory
    let wdir = homeDir <> "/.bk"
    let bookmarkCSVFile = wdir <> "/bk-bookmarks.csv"
    WBL.createDirectoryIfMissing False wdir
    bookmarkCSVFileExists <- WBL.doesFileExist bookmarkCSVFile
    when (not bookmarkCSVFileExists) $ 
        writeCSVFile bookmarkCSVFile Map.empty
    return bookmarkCSVFile

_handler :: Bool -> (Map.Map Text Bookmark  -> IO (Map.Map Text Bookmark)) -> IO ()
_handler writeMode action = 
    do bookmarkCSVFile <- initializeWorkDir     
       (errs,csvContents) <- readCSVFile bookmarkCSVFile       
       if null errs
       then do csvContents' <- action csvContents                                       
               when writeMode $ writeCSVFile bookmarkCSVFile csvContents'
               exitSuccess
       else print errs >> exitFailure
