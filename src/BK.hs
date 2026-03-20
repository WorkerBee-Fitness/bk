{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE  OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
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
     handler_) where

import Prelude.Linear
    ( Show,
      Bool(False, True),
      String,
      IO,
      Either,
      undefined,
      putStrLn,
      FilePath,
      Semigroup((<>)), otherwise )

import Prelude
    (($),
     (.),
     (<$>), id, Either (..), (++), error, Show (..), Foldable (..), Eq (..), Maybe)

import qualified System.IO as System
import qualified Control.Functor.Linear as Linear
import qualified System.IO.Resource.Linear as Linear
import qualified Data.Unrestricted.Linear as Linear
import Data.Text (Text, concat, pack, unpack)
import Data.ByteString.Lazy as BL hiding (null)
import Data.ByteString as BS hiding (null)
import GHC.Generics (Generic)
import Data.Csv.Incremental (Parser(..),decode, HasHeader (HasHeader))
import Data.Csv(FromRecord, ToRecord, FromField (..), Field, ToField (..), encode, encodeByName, ToNamedRecord (..), namedRecord, (.=), NamedRecord, Name)
import qualified Data.Vector as Vec

import qualified WBeeLib.ByteString as WBL
import qualified WBeeLib.Text as WBL
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (Monad(..), return, MonadPlus (mzero), when)
import Prelude (print, MonadFail (fail))
import GHC.IO.IOMode (IOMode(..))
import qualified System.IO.Linear as Linear
import qualified System.IO as Data.Text
import qualified Data.Map as Map

data BKType = BKAlias 
            | BKBookmark
    deriving (Generic, Show)

instance FromField  BKType where
    parseField = parseField' . parseBKType . WBL.byteStringToTextUTF8
        where
            parseField' (Right bt) = return bt
            parseField' (Left err) = fail err

parseBKType 
    :: Text
    -> Either String BKType
parseBKType s 
        | s == "alias"    = return BKAlias
        | s == "bookmark" = return BKBookmark
        | otherwise       = Left $ (Data.Text.unpack s) ++ " is not a valid bookmark type"

instance ToField BKType where
  toField :: BKType -> Field
  toField BKAlias = "alias"
  toField BKBookmark = "bookmark"

instance FromRecord BKType
instance ToRecord   BKType

data Bookmark = Bookmark {
    bkType   :: !BKType,
    bkLabel  :: !Text,    
    bkTarget :: !Text
} deriving (Generic, Show)

instance FromRecord Bookmark
instance ToRecord   Bookmark

header :: Vec.Vector Name
header = Vec.fromList 
    ["type",
     "label",
     "target"]

instance ToNamedRecord Bookmark where
    toNamedRecord :: Bookmark -> NamedRecord
    toNamedRecord b = namedRecord [
        "type" .= bkType b, 
        "label" .= bkLabel b, 
        "target" .= bkTarget b]

logDebug 
    :: String 
    -> Linear.RIO ()
logDebug !s = Linear.do
    h <- Linear.unsafeAcquire (Linear.return (Linear.Ur ())) (\_ -> Linear.return ())
    r <- Linear.unsafeFromSystemIOResource_ (\_ -> putStrLn s) h
    Linear.release r

feedCSVFile 
    :: (BS.ByteString -> Parser Bookmark)
    -> Linear.Handle %1
    -> Linear.RIO (Linear.Ur (Parser Bookmark), Linear.Handle)
feedCSVFile parserFam csvFile = Linear.do
    (Linear.Ur isEOF, csvFile') <- Linear.hIsEOF csvFile
    if isEOF
    then Linear.return (Linear.Ur (parserFam BS.empty),csvFile')
    else Linear.do
        (Linear.Ur line,csvFile'') <- Linear.hGetLine csvFile'
        let line' = WBL.textUTF8ToByteString (Data.Text.concat [line,(Data.Text.pack "\n")])
        let parser = parserFam line'
        Linear.return (Linear.Ur parser,csvFile'')

updateAcc :: ([String], Map.Map Text Bookmark)
          -> [Either String Bookmark]
          -> ([String], Map.Map Text Bookmark)
updateAcc acc = Prelude.foldl upAcc acc
    where
        upAcc (errs,bkMap) (Left errMsg)          = (errs++[errMsg],bkMap)
        upAcc (errs,bkMap) (Right b) = (errs,Map.insert (bkLabel b) b bkMap)

loop :: ([String], Map.Map Text Bookmark)
     -> Linear.Handle %1
     -> Parser Bookmark 
     -> Linear.RIO (Linear.Ur ([String], Map.Map Text Bookmark))
loop acc csvFile (Fail _ errMsg)   
    = Linear.do 
        Linear.release csvFile 
        Linear.return (Linear.Ur (updateAcc acc [Left errMsg]))

loop acc csvFile (Many rs parserFam) 
    = Linear.do         
        (Linear.Ur parser,csvFile'') <- feedCSVFile parserFam csvFile
        loop (updateAcc acc rs) csvFile'' parser                                       

loop acc csvFile (Done rs) 
    = Linear.do     
        Linear.release csvFile 
        Linear.return (Linear.Ur (updateAcc acc rs))

readCSVFile 
    :: FilePath 
    -> IO ([String], Map.Map Text Bookmark)
readCSVFile csvFilePath = Linear.run $ readCSVFile' 
    where
        readCSVFile' 
            :: Linear.RIO (Linear.Ur ([String], Map.Map Text Bookmark))
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

handler_ :: (Map.Map Text Bookmark  -> IO ()) -> IO ()
handler_ handle = _handler False (\m -> handle m >> return m)

handler :: (Map.Map Text Bookmark  -> IO (Map.Map Text Bookmark)) -> IO ()
handler = _handler True

_handler :: Bool -> (Map.Map Text Bookmark  -> IO (Map.Map Text Bookmark)) -> IO ()
_handler writeMode handle = 
    do (errs,csvContents) <- readCSVFile "test/test.csv"
       if null errs
       then do csvContents' <- handle csvContents                                       
               when writeMode $ writeCSVFile "test/test.csv" csvContents'
               exitSuccess
       else print errs >> exitFailure
