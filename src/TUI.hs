{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE ViewPatterns           #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE DataKinds              #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module TUI (mainLoop) where

--
-- * External Imports:
--
import Data.Text          (Text)
import System.Exit        (ExitCode(..)
                          ,exitFailure
                          ,exitWith)
import Data.Time.Calendar (Day)
import Data.Time          (getCurrentTime
                          ,UTCTime (..))
import Data.List          (uncons)
import System.Process     (readCreateProcessWithExitCode
                          ,shell)
import System.IO          (hPutStr
                          ,stdout
                          ,stderr)

import Options.Applicative  qualified as OptA
import Data.Attoparsec.Text qualified as Atto
import Data.Text            qualified as DT

--
-- * Internal Imports:                                     
-- 
import BK  (BKType (..)
           ,Bookmark (..)
           ,handler
           ,showBKType
           ,addBookmark
           ,bookmark
           ,handler_
           ,findBookmark
           ,removeBookmark
           ,maxOffsetBKMap
           ,recentBookmarks
           ,showBKMap
           ,filterBKMap
           ,isAlias
           ,isBookmark)

import qualified Lib
import qualified Data.Char as DT

_progName :: String
_progName = "bk"

_progVersion :: String
_progVersion = "v0.2"

--
-- * Option parsing
-- 

-- | All available command-line options.
data BKOption
    = OptAddBK BKType Text Text -- ^ Option "add" handles both bookmarks and aliases.
    | OptRunBK Text [Text]      -- ^ Option "run" handles running a bookmark with arguments.
    | OptRemoveBK Text          -- ^ Option "remove" handles removing a bookmark/alias.    
    | OptFindBK Text            -- ^ Option "find" searches for a bookmark.
    | OptList                   -- ^ Option "list" outputs all bookmarks and aliases to the terminal.
    | OptBookmarks              -- ^ Option "bookmarks" outputs all bookmarks to the terminal.    
    | OptAliases                -- ^ Option "aliases" outputs all aliases to the terminal.    
    | OptRecentsBK              -- ^ Option "recents" outputs the recently added bookmarks and aliases to the terminal.
    deriving (Show)

--
-- ** Attoparsec Parsers
-- 

-- | Parses an assignment of the form @label=target@.
-- Skips over whitespace before and after the @=@.
assignParser :: Atto.Parser (Text,Text)
assignParser = do 
    label <- labelParser
    Atto.skipSpace
    Atto.skip (=='=')
    Atto.skipSpace
    target <- targetParser
    return $ (label,target)

-- | Parses a bookmark label.
-- Labels must start with a letter and then be alpha-numeric with the addition of @_@ and @-@.
labelParser :: Atto.Parser Text
labelParser = do
    startChar <- Atto.take 1
    if DT.all DT.isAlpha startChar
    then do
        rest <- Atto.takeWhile1 $ DT.isAlphaNum `Lib.orF` (`elem` ['-','_'])
        return $ DT.append startChar rest
    else fail $ "parse error: bookmark labels must start with a letter."

-- | Parses the target of a bookmark.
-- Currently, there are no conditions on what a target can be, but in the future
-- we will need to modify this to handle variables. 
-- 
-- Reminder: the shell seems to be removing any quotes from the assignment.
targetParser :: Atto.Parser Text
targetParser = Atto.takeText            

-- | Parses an option that takes a label as input.
optLabelParser 
    :: (Text -> BKOption)    -- ^ Option we are parsing
    -> Atto.Parser BKOption
optLabelParser opt = do 
    label <- labelParser
    return $ opt label

-- | Parses an "add" option (`OptAddBK`) from an assignment argument.
optAddBKParser 
    :: BKType -- ^ Type of the bookmark being added
    -> Atto.Parser BKOption
optAddBKParser bkType = do 
    (label,target) <- assignParser
    return $ OptAddBK bkType label target

-- | Parses the @run@ option.
-- The list of arguments are parsed by another parser and then passed in via
-- applicative sequencing. See `bkRunCmdParser` implementation.
optRunBKParser 
    :: Atto.Parser ([Text] -> BKOption)
optRunBKParser = do 
    label <- labelParser
    return $ OptRunBK label

-- | Parses the @remove@ option.
optRemoveBKParser 
    :: Atto.Parser BKOption
optRemoveBKParser = optLabelParser OptRemoveBK

-- | Parses the @find@ option.
optFindBKParser 
    :: Atto.Parser BKOption
optFindBKParser = optLabelParser OptFindBK

--
-- ** Optparse-Applicative Parsers
-- 

-- | Imports the `optAddBKParser` into optparse-applicative as an argument
-- parser.
bkAddBkParser 
    :: BKType -- ^ Type of the bookmark being added
    -> OptA.Parser BKOption
bkAddBkParser bkType = OptA.argument (parser bkType) (OptA.metavar "LABEL=TARGET")
    where
        parser :: BKType -> OptA.ReadM BKOption
        parser bkType = OptA.eitherReader $ Atto.parseOnly (optAddBKParser bkType) . DT.pack

-- | Parses the subcommands of the @add@ option.
bkAddCmdParser :: OptA.Parser BKOption
bkAddCmdParser = OptA.hsubparser
    (  OptA.command "bookmark" (OptA.info (bkAddBkParser BKBookmark) (OptA.progDesc "Add a bookmark"))
    <> OptA.command "alias"    (OptA.info (bkAddBkParser BKAlias)    (OptA.progDesc "Add an alias"))
    )

-- | Parses the arguments to the run option.
bkRunCmdParser :: OptA.Parser BKOption
bkRunCmdParser = 
    (OptA.argument labelParser (OptA.metavar "LABEL")  
    <*> 
    (OptA.many $ OptA.argument argParser (OptA.metavar "ARGS")))
    where
        labelParser ::  OptA.ReadM ([Text] -> BKOption)
        labelParser = OptA.eitherReader $ Atto.parseOnly optRunBKParser . DT.pack

        argParser :: OptA.ReadM Text
        argParser = OptA.str

-- | Parses the arguments to the @remove@ command.
bkRemoveCmdParser :: OptA.Parser BKOption
bkRemoveCmdParser = 
    OptA.argument labelParser (OptA.metavar "LABEL")  
    where
        labelParser ::  OptA.ReadM BKOption
        labelParser = OptA.eitherReader $ Atto.parseOnly optRemoveBKParser . DT.pack

-- | Parses the argument to @find@ command.
bkFindCmdParser :: OptA.Parser BKOption
bkFindCmdParser = 
    OptA.argument labelParser (OptA.metavar "LABEL")  
    where
        labelParser ::  OptA.ReadM BKOption
        labelParser = OptA.eitherReader $ Atto.parseOnly optFindBKParser . DT.pack

-- | Parses the @list@ command.
bkListCmdParser :: OptA.Parser BKOption
bkListCmdParser = pure OptList

-- | Parses the @bookmarks@ command.
bkListBksCmdParser :: OptA.Parser BKOption
bkListBksCmdParser = pure OptBookmarks

-- | Parses the @aliases@ command.
bkListAliasesCmdParser :: OptA.Parser BKOption
bkListAliasesCmdParser = pure OptAliases

-- | Parses the @aliases@ command.
bkRecentsParser :: OptA.Parser BKOption
bkRecentsParser = pure OptRecentsBK

-- | Parses the various command-line options.
bkCmdParser :: OptA.Parser BKOption
bkCmdParser = OptA.hsubparser 
    (  OptA.command "add"       (OptA.info (bkAddCmdParser)         (OptA.progDesc "Add a bookmark or alias"))
    <> OptA.command "run"       (OptA.info (bkRunCmdParser)         (OptA.progDesc "Runs a bookmark or alias"))
    <> OptA.command "remove"    (OptA.info (bkRemoveCmdParser)      (OptA.progDesc "Removes a bookmark or alias"))
    <> OptA.command "find"      (OptA.info (bkFindCmdParser)        (OptA.progDesc "Searches for a bookmark or alias"))
    <> OptA.command "list"      (OptA.info (bkListCmdParser)        (OptA.progDesc "Lists all bookmarks and aliases"))
    <> OptA.command "bookmarks" (OptA.info (bkListBksCmdParser)     (OptA.progDesc "Lists all bookmarks"))
    <> OptA.command "aliases"   (OptA.info (bkListAliasesCmdParser) (OptA.progDesc "Lists all aliases"))
    )
    OptA.<|> bkRunCmdParser  -- run is the default when no commands are given.
    OptA.<|> bkRecentsParser -- No options or arguments, then show recents.

-- | Generates the various additional options and messages using the command
-- parser. This adds the @--help@ and @--version@ options as well as the program
-- description and header for the help message.
bkOpts :: OptA.ParserInfo BKOption
bkOpts = OptA.info (bkCmdParser  OptA.<**> OptA.helper OptA.<**> OptA.simpleVersioner _progVersion)
    (OptA.fullDesc <> OptA.progDesc _progName <> OptA.header "BeeKeeper remembers so you don't have to!")

--
-- * Option Handlers
-- 

-- | Runs an options handler.
handleOpt 
    :: BKOption -- ^ Option to handle
    -> IO ()
handleOpt OptRecentsBK      = handleRecentsbk
handleOpt OptList           = handleListBookmarks Nothing
handleOpt OptBookmarks      = handleListBookmarks (Just BKBookmark)
handleOpt OptAliases        = handleListBookmarks (Just BKAlias)
handleOpt (OptAddBK ty l t) = handleAddbk ty l t
handleOpt (OptFindBK l)     = handleFindbk l
handleOpt (OptRunBK l args) = handleRunbk l args
handleOpt (OptRemoveBK l)   = handleRemovebk l

-- | Handler for the @add@ option.
handleAddbk 
    :: BKType -- ^ Type of the bookmark being added
    -> Text   -- ^ Label of the bookmark being added
    -> Text   -- ^ Target of the bookmark being added
    -> IO ()
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
            = either
                (Lib.putStrLnStdErr . ("error: "<>))
                (\b -> handler $ \csvContents -> 
                         do homedir <- Lib.getHomeDirectory
                            either 
                                (\errMsg -> do Lib.putStrLnStdErr $ "error: " <> errMsg
                                               exitFailure)
                                (\updatedMap -> 
                                    do putStrLn $ "created " <> DT.unpack (showBKType typebk)  <> " \"" <> (DT.unpack labelbk) <> "\""
                                       return updatedMap)
                                $ addBookmark b homedir csvContents)
              $ bookmark typebk labelbk targetbk createdbk createdbk 

-- | Handler for the @find@ option.
handleFindbk 
    :: Text -- ^ Label of the bookmark to search for
    -> IO ()
handleFindbk labelbk = handler_
    (\csvContents -> case findBookmark labelbk csvContents of
                        Nothing -> Lib.putStrLnStdErr $ "bookmark not found " <> DT.show labelbk 
                        Just b ->  print b)

-- | Handler for the @remove@ option.
handleRemovebk 
    :: Text  -- ^ Label of the bookmark to remove
    -> IO ()
handleRemovebk labelbk = handler $ 
    \csvContents -> do
        let newMap = removeBookmark labelbk csvContents
        putStrLn $ "removed bookmark " <> show (DT.unpack labelbk)
        return newMap

-- | Handler for the @run@ option.
handleRunbk 
    :: Text   -- ^ Label of the bookmark to run
    -> [Text] -- ^ List of arguments to pass to the target
    -> IO ()
handleRunbk labelbk inArgs = handler_ $ \csvContents -> do
    case findBookmark labelbk csvContents of
        Nothing -> Lib.putStrLnStdErr $ "bookmark not found " <> DT.show labelbk
        Just b -> 
            case bkType b of
                BKBookmark -> putStrLn . DT.unpack . bkTarget $ b
                BKAlias -> do
                    let cmdM = uncons $ DT.words $ bkTarget b
                    maybe 
                        (Lib.putStrLnStdErr $ "error: target is empty for label \"" <> (bkLabel b) <> "\"")
                        (\(cmd',savedArgs) -> do let args = savedArgs <> inArgs
                                                 let cmd = DT.unpack . DT.unwords $ cmd':args
                                                 putStrLn $ "running \""<>cmd<>"\""                                                 
                                                 (exCode,out,err) <- readCreateProcessWithExitCode (shell cmd) ""
                                                 case exCode of
                                                    ExitSuccess -> do putStr out                                                                       
                                                                      exitWith ExitSuccess
                                                    ExitFailure i -> do hPutStr stdout out
                                                                        hPutStr stderr err
                                                                        exitWith $ ExitFailure i)
                        cmdM                    

-- | Handler for the @recents@ option.
handleRecentsbk :: IO ()
handleRecentsbk = handler_
    (\csvContents -> 
        do today <- getCurrentTime
           let maxLabelOffset = maxOffsetBKMap csvContents
           let (recAliases,recBks) = recentBookmarks (utctDay today) csvContents
           --TODO: Need to pull this out into a function def:
           putStrLn . DT.unpack  $ showBKMap maxLabelOffset recAliases
           putStrLn . DT.unpack  $ showBKMap maxLabelOffset recBks)

-- | Handler for the @list@ option.
-- If the input is @Nothing@ then list all of the bookmarks and aliases, but if
-- it's @Just bkType@ then filter the list based on the value of @bkType@.
handleListBookmarks 
    :: Maybe BKType -- ^ List filter
    -> IO ()
handleListBookmarks mbkType = handler_ $
    \csvContents -> 
        do let _map = filterBKMap (pred mbkType) csvContents
           let maxLabelOffset = maxOffsetBKMap _map
           putStrLn . DT.unpack  $ showBKMap maxLabelOffset _map
    where
        pred :: Maybe BKType -> Bookmark -> Bool
        pred Nothing _ = True
        pred (Just BKAlias) bk    = isAlias bk
        pred (Just BKBookmark) bk = isBookmark bk

-- | The main loop. 
-- This is called by the `Main` module.
mainLoop ::  IO ()
mainLoop = OptA.execParser bkOpts >>= handleOpt
