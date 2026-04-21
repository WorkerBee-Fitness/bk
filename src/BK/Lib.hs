{-# LANGUAGE ViewPatterns #-}
module BK.Lib 
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
    orF,
    tomlResult,
    concatErrors,
    left,
    right,
    exitFailureWithMsg) where

import Data.ByteString      qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.ByteString.UTF8 qualified as BSU
import Data.Text            qualified as DT
import Data.Text.Encoding   qualified as TSE
import System.FilePath      qualified as System.FP
import System.Directory     qualified as System.Dir
import Toml                 qualified as Toml

import Control.Monad.IO.Class (MonadIO (..))
import Data.List              (isPrefixOf)
import System.FilePath        ((</>))
import Data.Text.IO           (hPutStrLn)
import System.IO              (stderr, 
                               stdout)
import Data.Bool (bool)
import System.Exit (exitFailure)

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

-- Failure:

exitFailureWithMsg :: DT.Text -> IO r
exitFailureWithMsg msg = do
  putStrLnStdErr msg
  exitFailure


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

-- * Toml helpers
tomlResult :: Toml.Result e a -> ([e] -> r) -> ([e] -> a -> r) -> r
tomlResult (Toml.Failure e)   f _ = f e
tomlResult (Toml.Success e a) _ s = s e a

-- * String Helpers

-- | Concats a list of strings separating each one by `sep`.
strConcatSep 
    :: DT.Text   -- ^ Separator
    -> [String] -- ^ Strings to concat
    -> DT.Text
strConcatSep sep = foldl (\acc s -> DT.pack s <> sep <> acc) "" 

-- | Concats a list of warnings as @String@ using a newline separator.
concatErrors 
    :: [String] -- ^ List of warnings
    -> DT.Text
concatErrors = strConcatSep "\n"

-- | Either helpers
left :: Applicative m => e -> m (Either e r)
left = pure . Left

right :: Applicative m => r -> m (Either e r)
right = pure . Right