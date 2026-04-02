{-# LANGUAGE ViewPatterns #-}
module Lib 
    (FileSystemMonad(..),
    byteStringToTextUTF8, 
    byteStringToStringUTF8,
    textUTF8ToByteString,
    stringUTF8ToByteString,
    expandHomeDirectory,
    lazyByteStringToByteString,
    putStrLnStdOut,
    putStrLnStdErr,
    andF,
    orF) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.Text            as DT
import qualified Data.Text.Encoding   as TSE
import qualified System.FilePath      as System.FP
import qualified System.Directory     as System.Dir

import Control.Monad.IO.Class (MonadIO (..))
import Data.List              (isPrefixOf)
import System.FilePath        ((</>))
import Data.Text.IO           (hPutStrLn)
import System.IO              (stderr, 
                               stdout)

putStrLnStdErr 
    :: DT.Text 
    -> IO ()
putStrLnStdErr = hPutStrLn stderr 

putStrLnStdOut 
    :: DT.Text 
    -> IO ()
putStrLnStdOut = hPutStrLn stdout

textUTF8ToByteString 
    :: DT.Text 
    -> BS.ByteString
textUTF8ToByteString = TSE.encodeUtf8 

byteStringToTextUTF8 
    :: BS.ByteString 
    -> DT.Text
byteStringToTextUTF8 = TSE.decodeUtf8

byteStringToStringUTF8 
    :: BSU.ByteString 
    -> String
byteStringToStringUTF8 = BSU.toString 

stringUTF8ToByteString 
    :: String 
    -> BSU.ByteString
stringUTF8ToByteString = BSU.fromString

lazyByteStringToByteString 
    :: BL.ByteString 
    -> BS.ByteString
lazyByteStringToByteString = BL.toStrict

-- File System:

class MonadIO m => FileSystemMonad m where
    createDirectoryIfMissing :: Bool
                             -> FilePath
                             -> m ()
    createDirectoryIfMissing b = liftIO . System.Dir.createDirectoryIfMissing b

    getHomeDirectory :: m FilePath
    getHomeDirectory = liftIO System.Dir.getHomeDirectory
    
    doesDirectoryExist :: FilePath
                       -> m Bool
    doesDirectoryExist = liftIO . System.Dir.doesDirectoryExist

    doesFileExist :: FilePath 
                  -> m Bool
    doesFileExist = liftIO . System.Dir.doesFileExist

instance FileSystemMonad IO

expandHomeDirectory 
    :: FilePath 
    -> FilePath 
    -> FilePath
expandHomeDirectory homedir (System.FP.normalise->path)
    | ['~',System.FP.pathSeparator] `isPrefixOf` path = homedir </> (drop 2 path)
    | "~" `isPrefixOf` path = homedir </> (drop 1 path)
expandHomeDirectory _ path = path

-- Booleans:
andF :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
andF f g x = (f x) && (g x)

orF :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
orF f g x = (f x) || (g x)