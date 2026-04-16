{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
module BK.Setup 
    (setup) where

-- | * External Imports

import Data.Text              (Text)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath        ((</>))
import System.Exit            (exitFailure)

import qualified Data.Text as DT
import qualified System.Environment as Env
import qualified System.Console.Haskeline as HL

-- | * Internal Imports
import BK.BKMap qualified as BK
import BK.Lib   qualified as Lib
import Control.Monad (when)

-- | * Setup

-- | Set to @True@ to turn on debug logging.
_debugSetup :: Bool
_debugSetup = False

-- | Setup BK.
-- Discovers the BK configuration, but if this is the first time running BK,
-- then we enter an interactive setup process. 
-- Nonrecoverable errors exit with failure.
setup :: IO BKConfigPaths
setup = do
  -- Get $BK_CONFIG_DIR from the environment, if it isn't set, then continue
  -- with $HOME as the config path.
  Env.lookupEnv "BK_CONFIG_DIR" 
    >>= maybe 
          (Lib.getHomeDirectory >>= continueSetup)
          continueSetup 

exitFailureWithMsg :: Text -> IO r
exitFailureWithMsg msg = do
  Lib.putStrLnStdErr msg
  exitFailure

-- | Supported configuration files.
-- These are all stored under the @$CONFIG_DIR/bk@ directory.
data BKConfigPaths = BKConfigPaths {
   -- | Path to the configuration directory
   bkConfigDirectoryPath :: FilePath
   -- | Path to the booksmarks CSV file.
  ,bkBookmarksFile :: FilePath
} deriving Show

-- | Returns the absolute paths to the config files.
bkConfigPaths 
  :: FilePath          -- ^ Path to the configuration directory
  -> BKConfigPaths
bkConfigPaths configDir = BKConfigPaths {
   bkConfigDirectoryPath = configDir
  ,bkBookmarksFile       = configDir </> "bookmarks.csv"
}

-- | Convenience function for building the path to the bookmarks CSV file.
getBKBookmarksFilePath 
  :: FilePath -- ^ Path to the configuration directory
  -> FilePath
getBKBookmarksFilePath = bkBookmarksFile . bkConfigPaths

-- | Tests whether all of the configuration files exist in the configuration directory.
doConfigFilesExist 
  :: FilePath                       -- ^ Path to the configuration directory
  -> IO (Either Text BKConfigPaths) -- ^ Absolute paths to all of the configuration files.
doConfigFilesExist configDir = do
  d <- Lib.doesDirectoryExist configDir  
  if d
  then do let configFilesPaths = BKConfigPaths { 
                 bkConfigDirectoryPath = configDir
                ,bkBookmarksFile       = (getBKBookmarksFilePath configDir) 
              }
          f2 <- Lib.doesFileExist $ bkBookmarksFile configFilesPaths
          return $ if f2
                   then Right $ configFilesPaths
                   else Left "missing CSV file"
  else return . Left $ "no such file or directory '"<>DT.pack configDir<>"'"

-- | Continues the setup to either an interactive setup if this is the first
-- time running @bk@ and initializing the configuration directory, or just
-- initializing the configuration paths.
continueSetup 
  :: FilePath    -- ^ Path to the configuration directory.
  -> IO BKConfigPaths
continueSetup configDir = doConfigFilesExist configDir        
                      >>= either goInteractive return  
  where
    goInteractive errMsg = do
      when _debugSetup (Lib.putStrLnStdErr errMsg)
      interactiveSetup

isYesResponse :: String -> Bool
isYesResponse "" = True
isYesResponse "y"= True
isYesResponse "Y"= True
isYesResponse _   = False

isNoResponse :: String -> Bool
isNoResponse "" = True
isNoResponse "N" = True
isNoResponse "n" = True
isNoResponse _   = False

promptYesNo :: Text -> HL.InputT IO r -> HL.InputT IO r -> HL.InputT IO r
promptYesNo promptPrefix g f = loop
  where
    prompt = promptPrefix <> " [Yn]? "
    loop = do (HL.getInputLine $ DT.unpack prompt) >>= maybe loop
                (\c -> if isYesResponse c
                       then f
                       else if isNoResponse c
                            then g
                            else loop)    

-- | Prompts for a directory path.
-- Checks to see if the directory exists before returning. If it doesn't exist,
-- issue an error message and prompt for a new path. 
promptForFilePath 
  :: Text                  -- ^ Prompt prefix
  -> FilePath              -- ^ Default value
  -> HL.InputT IO FilePath
promptForFilePath prefix defaultValue = _promptForFilePath 
  where
    prompt = DT.unpack prefix <> " ["<>defaultValue<>"]" <> "? "
    _promptForFilePath = do
      HL.getInputLine prompt >>= maybe _promptForFilePath
        (\s -> case s of 
                "" -> return defaultValue
                _ -> do b <- liftIO $ Lib.doesDirectoryExist s
                        if b
                        then return s
                        else do liftIO $ Lib.putStrLnStdOut "\nPlease choose a directory that exists."
                                _promptForFilePath)

-- | Interactive setup that configures @bk@ for the first time.
interactiveSetup :: IO BKConfigPaths
interactiveSetup = HL.runInputT HL.defaultSettings $ loop 
  where     
    loop = do
      liftIO $ Lib.putStrLnStdOut $ "Welcome to the Beekeeper (bk) Interactive Setup!\n"
      promptYesNo 
        "Is this the first time running bk"
        (liftIO $ exitFailureWithMsg secondRunMessage) $
          do homedir <- liftIO Lib.getHomeDirectory
             configDirParent <- promptForFilePath 
                                  "Enter the directory where you would like to store bk's configuration directory" 
                                  homedir
             let configFilePaths = bkConfigPaths $ configDirParent </> "bk"
             liftIO $ initializeWorkDir configFilePaths
             return configFilePaths
  
    secondRunMessage :: Text
    secondRunMessage = 
         "Since you are seeing this setup for the second time, then you most likely\n"
      <> "setup a custom location to store the configuration files, and bk can no\n"
      <> "longer read the environment variable $BK_CONFIG_DIR.\n\n"
      <> "Please check that your shells configuration is exporting $BK_CONFIG_DIR"

initializeWorkDir :: BKConfigPaths -> IO ()
initializeWorkDir configFilesPaths = do 
    let configDir = bkConfigDirectoryPath configFilesPaths
    let csvFile   = bkBookmarksFile       configFilesPaths
    Lib.createDirectoryIfMissing False configDir    
    b <- Lib.doesFileExist csvFile
    when b $ do exitFailureWithMsg $ "Aborting setup!\n"
                                  <> "It seems bk is already setup, because " 
                                  <> DT.pack csvFile
                                  <> " already exists."
    BK.writeCSVFile csvFile BK.emptyBKMap
