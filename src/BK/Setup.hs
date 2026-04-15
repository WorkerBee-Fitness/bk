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

-- | * Setup

-- | Setup BK.
-- Discovers the BK configuration, but if this is the first time running BK,
-- then we enter an interactive setup process. 
-- Nonrecoverable errors exit with failure.
setup :: IO BkConfigFilePaths
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
data BkConfigFilePaths = BKConfigFilePaths {
  -- | Path to the configuration directory
  bkConfigDirectoryPath :: FilePath
  -- | Path to the TOML configuration file.
  ,bkConfigFile :: FilePath
  -- | Path to the booksmarks CSV file.
  ,bkBookmarksFile :: FilePath
}

-- | Returns the absolute paths to the config files.
bkConfigFilePaths 
  :: FilePath          -- ^ Path to the configuration directory
  -> BkConfigFilePaths
bkConfigFilePaths configDir = BKConfigFilePaths {
   bkConfigDirectoryPath = configDir
  ,bkConfigFile          = configDir </> "bk.toml"
  ,bkBookmarksFile       = configDir </> "bookmarks.csv"
}

-- | Convenience function for building the path to the TOML configuration file.
getBKConfigFilePath 
  :: FilePath -- ^ Path to the configuration directory
  -> FilePath
getBKConfigFilePath = bkConfigFile . bkConfigFilePaths

-- | Convenience function for building the path to the bookmarks CSV file.
getBKBookmarksFilePath 
  :: FilePath -- ^ Path to the configuration directory
  -> FilePath
getBKBookmarksFilePath = bkBookmarksFile . bkConfigFilePaths

-- | Tests whether all of the configuration files exist in the configuration directory.
doConfigFilesExist 
  :: FilePath                     -- ^ Path to the configuration directory
  -> IO (Maybe BkConfigFilePaths) -- ^ Absolute paths to all of the configuration files.
doConfigFilesExist configDir = do
  d <- Lib.doesDirectoryExist configDir  
  if d
  then do let configFilesPaths = BKConfigFilePaths { 
                 bkConfigDirectoryPath = configDir
                ,bkConfigFile          = (getBKConfigFilePath    configDir) 
                ,bkBookmarksFile       = (getBKBookmarksFilePath configDir) 
              }
          f1 <- Lib.doesFileExist $ bkConfigFile configFilesPaths 
          f2 <- Lib.doesFileExist $ bkBookmarksFile configFilesPaths
          return $ if f1 && f2
                   then Just $ configFilesPaths
                   else Nothing
  else return Nothing

-- | Continues the setup to either an interactive setup if this is the first
-- time running @bk@ and initializing the configuration directory, or just
-- initializing the configuration directory.
continueSetup 
  :: FilePath    -- ^ Path to the configuration directory.
  -> IO BkConfigFilePaths
continueSetup configDir = 
      doConfigFilesExist configDir 
  >>= maybe interactiveSetup return  

isYesResponse :: Char -> Bool
isYesResponse '\n' = True
isYesResponse 'y' = True
isYesResponse 'Y' = True
isYesResponse _   = False

isNoResponse :: Char -> Bool
isNoResponse 'n' = True
isNoResponse 'N' = True
isNoResponse _   = False

promptYesNo :: Text -> HL.InputT IO r -> HL.InputT IO r -> HL.InputT IO r
promptYesNo promptPrefix g f = do
  let prompt = promptPrefix <> "[Yn]? "
  (HL.getInputChar $ DT.unpack prompt) >>= maybe (promptYesNo prompt f g)
      (\c -> if isYesResponse c
             then f
             else if isNoResponse c
                  then g
                  else promptYesNo prompt f g)

-- | Prompts for a directory path.
-- Checks to see if the directory exists before returning. If it doesn't exist,
-- issue an error message and prompt for a new path. 
promptForFilePath 
  :: Text                  -- ^ Prompt prefix
  -> FilePath              -- ^ Default value
  -> HL.InputT IO FilePath
promptForFilePath prefix defaultValue = _promptForFilePath 
  where
    _promptForFilePath = do
      let prompt = DT.unpack prefix <> "? "
      HL.getInputLineWithInitial prompt (defaultValue,"") >>= maybe _promptForFilePath
        (\s -> case s of 
                "" -> return defaultValue
                _ -> do b <- liftIO $ Lib.doesDirectoryExist s
                        if b
                        then return s
                        else do liftIO $ Lib.putStrLnStdOut "directory doesn't exist"
                                _promptForFilePath)

-- | Interactive setup that configures @bk@ for the first time.
interactiveSetup :: IO BkConfigFilePaths
interactiveSetup = HL.runInputT HL.defaultSettings $ loop 
  where     
    loop = do
      liftIO $ Lib.putStrLnStdOut $ "Welcome to the Beekeeper (bk) Interactive Setup!"
      promptYesNo 
        "Is this the first time running bk?"
        (liftIO $ exitFailureWithMsg secondRunMessage) $
          do homedir <- liftIO Lib.getHomeDirectory
             configDirParent <- promptForFilePath 
                                  "Enter the directory where you would like to store bk's configuration files" 
                                  homedir
             let configFilePaths = bkConfigFilePaths $ configDirParent </> "bk"
             liftIO $ initializeWorkDir configFilePaths
             return configFilePaths
  
    secondRunMessage :: Text
    secondRunMessage = 
         "Since you are seeing this setup for the second time, then you most likely\n"
      <> "setup a custom location to store the configuration files, and bk can no\n"
      <> "longer read the environment variable $BK_CONFIG_DIR.\n\n"
      <> "Please check that your shells configuration is exporting $BK_CONFIG_DIR"

initializeWorkDir :: BkConfigFilePaths -> IO ()
initializeWorkDir configFilesPaths = do 
    Lib.createDirectoryIfMissing False $ bkConfigDirectoryPath configFilesPaths    
    BK.writeCSVFile (bkBookmarksFile configFilesPaths) BK.emptyBKMap
