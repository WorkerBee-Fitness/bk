{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DataKinds #-}
module TUI (mainLoop) where

import System.Environment (getArgs)
import System.Process (spawnCommand)
import BK (addBookmark, Bookmark (..), removeBookmark, findBookmark, handler, handler_, BKType (..), parseBKType)
import Data.Text (pack, Text, split, unpack)
import Control.Monad (void)
import System.IO (hFlush, stdout)

_progName :: String
_progName = "bk"

_progVersion :: String
_progVersion = "0.0.0.1"

data BKOption 
    = OptAddBK BKType Text Text
    | OptRunBK Text
    | OptRemoveBK Text
    | OptFindBK Text
    | OptHelpBK 
    | OptVersionBK
    deriving (Show)

helpString :: String
helpString = 
       "BeeKeeper remembers so you don't have to!\n\n"
    ++ "Usage:\n\t"++_progName++" [command-line-options]"++"\n\n"
    ++ "Version:\n\t"++_progName++"-"++_progVersion++"\n\n"
    ++ "Options:\n"        
        ++"\tadd    bookmark LABEL=TARGET | add a new bookmark called LABEL that points to TARGET\n"
        ++"\tadd    alias    LABEL=TARGET | add a new alias called LABEL that points to TARGET\n"
        ++"\n"
        ++"\trun    LABEL                 | runs the alias LABEL if it exists\n"
        ++"\tfind   LABEL                 | returns the TARGET of the bookmark/alias LABEL if it exists\n"
        ++"\n"
        ++"\tremove LABEL                 | removes the bookmark/alias called LABEL if it exists\n"
        ++"\n"
        ++"\thelp                         | returns this help message\n"
        ++"\tversion                      | returns the current version"

parseBKAssignment :: Text -> Either String (Text,Text)
parseBKAssignment s = aux $ split (=='=') s
    where
        aux :: [Text] -> Either String (Text,Text)       
        aux [l,t] = Right $ (l,t)
        aux _     = Left $ "invalid bookmark assignment: "++unpack s

parseBKAdd :: [Text] -> Either String BKOption
parseBKAdd [bkTypeStr,bkAssignStr] 
    = case (_bkType,_bkAssign) of
        (Right ty,Right (l,tar)) -> Right $ OptAddBK ty l tar
        (Left err1, Right _) -> Left err1
        (Right _, Left err2) -> Left err2
        (Left err1, Left err2) -> Left $ err1 ++ "\n" ++ err2
    where
        _bkType = parseBKType bkTypeStr
        _bkAssign = parseBKAssignment bkAssignStr
parseBKAdd _ = Left $ "invalid number of arguments given to add"

parseBKRemove :: [Text] -> Either String BKOption
parseBKRemove [label] = Right $ OptRemoveBK label
parseBKRemove _ = Left $ "invalid number of arguments given to remove"

parseBKFind :: [Text] -> Either String BKOption
parseBKFind [label] = Right $ OptFindBK label
parseBKFind _ = Left $ "invalid number of arguments given to find"

parseBKRun :: [Text] -> Either String BKOption
parseBKRun [label] = Right $ OptRunBK label
parseBKRun _       = Left $ "invalid number of arguments given to run"

parseOpt :: [Text] -> Either String BKOption
parseOpt []              = Right OptHelpBK
parseOpt ["help"]        = Right OptHelpBK
parseOpt ["version"]     = Right OptVersionBK
parseOpt ("add":args)    = parseBKAdd args
parseOpt ("find":args)   = parseBKFind args
parseOpt ("run":args)    = parseBKRun args
parseOpt ("remove":args) = parseBKRemove args
parseOpt ["-v"]          = Right OptVersionBK
parseOpt ["--version"]   = Right OptVersionBK
parseOpt ["-h"]          = Right OptHelpBK
parseOpt ["--help"]      = Right OptHelpBK
parseOpt args@[_]        = parseBKRun args
parseOpt (s:_)           = Left $ "invalid option: "++(unpack s)

handleOpt :: BKOption -> IO ()
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
handleAddbk ty l t = handleAddbk' ty l t
    where
        handleAddbk' :: BKType -> Text -> Text -> IO ()
        handleAddbk' typebk labelbk targetbk 
            = let b = Bookmark { bkType = typebk, bkLabel = labelbk, bkTarget = targetbk } 
               in handler (return . addBookmark b)

handleFindbk :: Text -> IO ()
handleFindbk labelbk = handler_
    (\csvContents -> case findBookmark labelbk csvContents of
                        Nothing -> putStrLn $ "bookmark not found " ++ (show labelbk)
                        Just b ->  print b)

handleRemovebk :: Text -> IO ()
handleRemovebk labelbk = handler (return . removeBookmark labelbk)

handleRunbk :: Text -> IO ()
handleRunbk labelbk = handler_ $ \csvContents -> do
    case findBookmark labelbk csvContents of
        Nothing -> putStrLn $ "alias not found " ++ (show labelbk)
        Just b -> 
            case bkType b of
                BKBookmark -> putStrLn $ (show labelbk) ++ " is not an alias"
                BKAlias -> do
                    let target = unpack . bkTarget $ b
                    putStrLn $ "running "++(show target)
                    hFlush stdout
                    void $ spawnCommand target

mainLoop ::  IO ()
mainLoop = do
    s <- getArgs
    case parseOpt $ map pack s of
        Right opt -> handleOpt opt
        Left err -> error err
