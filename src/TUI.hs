{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DataKinds              #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module TUI (mainLoop) where

-- External Imports:
import           System.Environment (getArgs)
import           System.Process     (spawnCommand)
import           Control.Monad      (void)
import           Data.Time.Calendar (Day)
import           Data.Text          (Text)
import           System.IO          (hFlush, 
                                     stdout)
import           Data.Time          (UTCTime (..),
                                     getCurrentTime)
import qualified Data.Text as DT

-- Internal Imports:                                     
import           WBeeLib.System.IO  (putStrLnStdErr)
import           BK                 (Bookmark (..), 
                                     BKType (..),
                                     addBookmark, 
                                     removeBookmark, 
                                     findBookmark, 
                                     handler, 
                                     handler_,                                       
                                     parseBKType, 
                                     recentBookmarks, 
                                     showBKMap, 
                                     maxOffsetBKMap, 
                                     showBKType, 
                                     filterBKMap, 
                                     isAlias, 
                                     isBookmark)

_progName :: String
_progName = "bk"

_progVersion :: String
_progVersion = "0.0.0.1"

data BKOption 
    = OptAddBK BKType Text Text
    | OptRunBK Text
    | OptRemoveBK Text
    | OptFindBK Text
    | OptList
    | OptBookmarks    
    | OptAliases
    | OptRecentsBK
    | OptHelpBK 
    | OptVersionBK
    deriving (Show)

helpString :: String
helpString = 
       "BeeKeeper remembers so you don't have to!\n\n"
    ++ "Usage:\n"
        ++"\t"++_progName++"                            | see recent activity\n"
        ++"\t"++_progName++" LABEL                      | runs the alias LABEL if it exists\n"
        ++"\t"++_progName++" [command-line-options]     | run a command-line-option\n\n"
    ++ "Version:\n\t"++_progName++"-"++_progVersion++"\n\n"
    ++ "Options:\n"        
        ++"\tadd    bookmark LABEL=TARGET | add a new bookmark called LABEL that points to TARGET\n"
        ++"\tadd    alias    LABEL=TARGET | add a new alias called LABEL that points to TARGET\n"
        ++"\n"
        ++"\trun    LABEL                 | runs the alias LABEL if it exists\n"
        ++"\tfind   LABEL                 | returns the TARGET of the bookmark/alias LABEL if it exists\n"
        ++"\tremove LABEL                 | removes the bookmark/alias called LABEL if it exists\n"
        ++"\n"
        ++"\tlist                         | lists all bookmarks and aliases\n"
        ++"\tbookmarks                    | lists all bookmarks\n"
        ++"\taliases                      | lists all aliases"
        ++"\n"
        ++"\thelp                         | returns this help message\n"
        ++"\tversion                      | returns the current version"

parseBKAssignment :: Text -> Either Text (Text,Text)
parseBKAssignment s = aux $ DT.split (=='=') s
    where
        aux :: [Text] -> Either Text (Text,Text)       
        aux [l,t] = Right $ (l,t)
        aux _     = Left $ "invalid bookmark assignment: " <> s

parseBKAdd :: [Text] -> Either Text BKOption
parseBKAdd [bkTypeStr,bkAssignStr] 
    = case (_bkType,_bkAssign) of
        (Right ty,Right (l,tar)) -> Right $ OptAddBK ty l tar
        (Left err1, Right _) -> Left err1
        (Right _, Left err2) -> Left err2
        (Left err1, Left err2) -> Left $ err1 <> "\n" <> err2
    where
        _bkType = parseBKType bkTypeStr
        _bkAssign = parseBKAssignment bkAssignStr
parseBKAdd _ = Left $ "invalid number of arguments given to add"

parseBKRemove :: [Text] -> Either Text BKOption
parseBKRemove [label] = Right $ OptRemoveBK label
parseBKRemove _ = Left $ "invalid number of arguments given to remove"

parseBKFind :: [Text] -> Either Text BKOption
parseBKFind [label] = Right $ OptFindBK label
parseBKFind _ = Left $ "invalid number of arguments given to find"

parseBKRun :: [Text] -> Either Text BKOption
parseBKRun [label] = Right $ OptRunBK label
parseBKRun _       = Left  $ "invalid number of arguments given to run"

parseOpt :: [Text] -> Either Text BKOption
parseOpt []              = Right OptRecentsBK
parseOpt ["help"]        = Right OptHelpBK
parseOpt ["version"]     = Right OptVersionBK
parseOpt ["list"]        = Right OptList
parseOpt ["bookmarks"]   = Right OptBookmarks
parseOpt ["aliases"]     = Right OptAliases
parseOpt ("add":args)    = parseBKAdd args
parseOpt ("find":args)   = parseBKFind args
parseOpt ("run":args)    = parseBKRun args
parseOpt ("remove":args) = parseBKRemove args
parseOpt ["-v"]          = Right OptVersionBK
parseOpt ["--version"]   = Right OptVersionBK
parseOpt ["-h"]          = Right OptHelpBK
parseOpt ["--help"]      = Right OptHelpBK
parseOpt args@[_]        = parseBKRun args
parseOpt (s:_)           = Left $ "invalid option: " <> s

handleOpt :: BKOption -> IO ()
handleOpt OptRecentsBK      = handleRecentsbk
handleOpt OptList           = handleListBookmarks Nothing
handleOpt OptBookmarks      = handleListBookmarks (Just BKBookmark)
handleOpt OptAliases        = handleListBookmarks (Just BKAlias)
handleOpt (OptAddBK ty l t) = handleAddbk ty l t
handleOpt (OptFindBK l)     = handleFindbk l
handleOpt (OptRunBK l)      = handleRunbk l
handleOpt (OptRemoveBK l)   = handleRemovebk l
handleOpt OptHelpBK         = handleHelp
handleOpt OptVersionBK      = handleVersion

handleHelp :: IO ()
handleHelp = putStrLn helpString

handleVersion :: IO ()
handleVersion = putStrLn $ _progName++"-"++_progVersion

handleAddbk :: BKType -> Text -> Text -> IO ()
handleAddbk ty l t = do
    createdDay <- today
    handleAddbk' ty l t createdDay
    where
        today :: IO Day
        today = do
            currentUTCTime <- getCurrentTime
            return . utctDay $ currentUTCTime

        handleAddbk' :: BKType -> Text -> Text -> Day -> IO ()
        handleAddbk' typebk labelbk targetbk createdbk
            = let b = Bookmark { 
                        bkType = typebk, 
                        bkLabel = labelbk, 
                        bkTarget = targetbk, 
                        bkCreated = createdbk, 
                        bkLastUsed = createdbk 
                      } 
               in handler $ \csvContents -> do
                    let newMap = addBookmark b csvContents
                    putStrLn $ "created " <> DT.unpack (showBKType typebk)  <> " " <> show (DT.unpack labelbk)
                    return newMap

handleFindbk :: Text -> IO ()
handleFindbk labelbk = handler_
    (\csvContents -> case findBookmark labelbk csvContents of
                        Nothing -> putStrLnStdErr $ "bookmark not found " <> DT.show labelbk 
                        Just b ->  print b)

handleRemovebk :: Text -> IO ()
handleRemovebk labelbk = handler $ 
    \csvContents -> do
        let newMap = removeBookmark labelbk csvContents
        putStrLn $ "removed bookmark " <> show (DT.unpack labelbk)
        return newMap

handleRunbk :: Text -> IO ()
handleRunbk labelbk = handler_ $ \csvContents -> do
    case findBookmark labelbk csvContents of
        Nothing -> putStrLnStdErr $ "alias not found " <> DT.show labelbk
        Just b -> 
            case bkType b of
                BKBookmark -> putStrLnStdErr $ (DT.show labelbk) <> " is not an alias"
                BKAlias -> do
                    let target = DT.unpack . bkTarget $ b
                    putStrLn $ "running "++(show target)
                    hFlush stdout
                    void $ spawnCommand target

handleRecentsbk :: IO ()
handleRecentsbk = handler_
    (\csvContents -> 
        do today <- getCurrentTime
           let maxLabelOffset = maxOffsetBKMap csvContents
           let (recAliases,recBks) = recentBookmarks (utctDay today) csvContents
           putStrLn . DT.unpack  $ showBKMap maxLabelOffset recAliases
           putStrLn . DT.unpack  $ showBKMap maxLabelOffset recBks)

handleListBookmarks :: Maybe BKType -> IO ()
handleListBookmarks mbkType = handler_ $
    \csvContents -> 
        do let _map = filterBKMap (pred mbkType) csvContents
           let maxLabelOffset = maxOffsetBKMap _map
           putStrLn . DT.unpack  $ showBKMap maxLabelOffset _map
    where
        pred :: Maybe BKType -> Bookmark -> Bool
        pred Nothing _ = True
        pred (Just BKAlias) bk = isAlias bk
        pred (Just BKBookmark) bk = isBookmark bk

mainLoop ::  IO ()
mainLoop = do
    s <- getArgs
    case parseOpt $ map DT.pack s of
        Right opt -> handleOpt opt
        Left err -> putStrLnStdErr err
