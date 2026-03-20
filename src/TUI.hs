{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE DataKinds #-}
module TUI
    (mainLoop) where

import Data.Data 
    (Typeable, 
     Data)

import System.Console.CmdArgs
    (Default (def),
     (&=),
     summary,
     help,
     cmdArgsMode,
     cmdArgsRun,
     args,
     modes, auto, Mode)
import System.Environment (withArgs, getArgs)
import Control.Monad (void)
import System.Console.CmdArgs (CmdArgs)
import BK (addBookmark, Bookmark (..), removeBookmark, findBookmark, handler, handler_, BKType (..), parseBKType)
import qualified WBeeLib.ByteString as WBL
import Data.Text (pack, Text, split, unpack, splitOn)
import System.Exit (exitFailure)
import qualified Data.ByteString as BS
import Data.Kind (Type)
import System.Console.CmdArgs.GetOpt (getOpt)
import qualified WBeeLib.Text as WBL

progName :: String
progName = "bk"

progVersion :: String
progVersion = "0.0.0.1"

data BKOption 
    = OptAddBK BKType Text Text
    | OptRemoveBK Text
    | OptFindBK Text
    deriving (Show)

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

parseOpt :: [Text] -> Either String BKOption
parseOpt [] = Left $ "help coming soon"
parseOpt ("add":args) = parseBKAdd args
parseOpt ("find":args) = parseBKFind args
parseOpt ["help"] = Left $ "help coming soon"
parseOpt ("remove":args) = parseBKRemove args
parseOpt (s:_) = Left $ "invalid option: "++(unpack s)

handleOpt :: BKOption -> IO ()
handleOpt (OptAddBK ty l t) = handleAddbk ty l t
handleOpt (OptRemoveBK l)   = handleRemovebk l
handleOpt (OptFindBK l)     = handleFindbk l

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
handleRunbk _labelbk = undefined

mainLoop ::  IO ()
mainLoop = do
    s <- getArgs
    case parseOpt $ map pack s of
        Right opt -> handleOpt opt
        Left err -> error err
