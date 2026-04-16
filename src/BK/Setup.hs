{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
module BK.Setup 
    (setup) where

-- | * External Imports

import Data.Text              (Text)
import Control.Monad          (when)
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

-- | Set to @True@ to turn on debug logging.
_debugSetup :: Bool
_debugSetup = False

-- | Setup BK.
-- Discovers the BK configuration, but if this is the first time running BK,
-- then we enter an interactive setup process. 
-- Nonrecoverable errors exit with failure.
setup :: IO BKConfigPaths
setup = 
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

squareBracket :: Text -> Text 
squareBracket t = "["<>t<>"]"

questionMark :: Text
questionMark = "?"

promptForShell :: Text -> HL.InputT IO Shell
promptForShell prefix = do
        osShellM <- liftIO $ Env.lookupEnv "SHELL" 
        let defaultValue = DT.pack $ maybe "" id osShellM
        liftIO $ when (DT.null defaultValue) $ exitFailureWithMsg "Aborting setup!\nUnable to determine shell"
        let prompt = DT.unpack $ prefix <> space <> squareBracket "bzf" <> questionMark <> space
        _promptForShell prompt defaultValue
  where        
    _promptForShell prompt defaultValue = do
        liftIO $ Lib.putStrLnStdOut $ 
                 newline 
               <> "Shell configuration"<>newline
               <> "Supported shells: [b]ash, [z]sh, and [f]ish" <> newline
               <> "Your current shell is "<>defaultValue
        HL.getInputLine prompt
          >>= maybe (_promptForShell prompt defaultValue)
                (\sh -> case sh of
                          "" -> _
                          "b" -> return Bash
                          "z" -> return Zsh
                          "f" -> return Fish
                          _ -> _promptForShell prompt defaultValue)

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
             let configFilePaths = bkConfigPaths $ configDirParent 
                               </> if configDirParent == homedir
                                   then ".bk"
                                   else "bk"
             shell <- promptForShell "Which shell should be initialized"
             liftIO $ initializeConfigDir configFilePaths shell
             return configFilePaths
  
    secondRunMessage :: Text
    secondRunMessage = 
         "Since you are seeing this setup for the second time, then you most likely\n"
      <> "setup a custom location to store the configuration files, and bk can no\n"
      <> "longer read the environment variable $BK_CONFIG_DIR.\n\n"
      <> "Please check that your shells configuration is exporting $BK_CONFIG_DIR"

initializeConfigDir :: BKConfigPaths -> Shell -> IO ()
initializeConfigDir configFilesPaths sh = do 
    let configDir = bkConfigDirectoryPath configFilesPaths
    let csvFile   = bkBookmarksFile       configFilesPaths
    b <- Lib.doesFileExist csvFile
    when b $ do exitFailureWithMsg $ "Aborting setup!\n"
                                  <> "It seems bk is already setup, because " 
                                  <> DT.pack csvFile
                                  <> " already exists."
    Lib.createDirectoryIfMissing False configDir
    createCompletionsDir sh 
    BK.writeCSVFile csvFile BK.emptyBKMap
  where
    createCompletionsDir Fish = return ()
    createCompletionsDir _sh = do
      homedir <- Lib.getHomeDirectory
      let path = completionsDirPath homedir configFilesPaths _sh
      Lib.createDirectoryIfMissing False path

-- | Supported shell initialization.
data Shell = Zsh 
           | Bash 
           | Fish

doubleQuote :: Text -> Text
doubleQuote t = "\""<>t<>"\""

singleQuote :: Text -> Text
singleQuote t = "'"<>t<>"'"

space :: Text 
space = " "

colon :: Text
colon = ":"

newline :: Text
newline = "\n"

exportEnvar :: Shell -> (Text,Text) -> Text
exportEnvar Fish (var,value) = "set -x "<>var<>space<>doubleQuote value
exportEnvar _    (var,value) = "export "<>var<>"="<>doubleQuote value

updatePath :: Shell -> Text -> Text
updatePath Fish dir = "fish_add_path "<>doubleQuote dir
updatePath sh   dir = exportEnvar sh ("PATH",doubleQuote $ dir<>colon<>"$PATH")

sourceScript :: Text -> Text
sourceScript script = "source " <> script

updateZshFPath :: FilePath -> Text
updateZshFPath (DT.pack->dir) = "fpath=("<>doubleQuote dir<>" $fpath)"

compinitZsh :: Text
compinitZsh = "autoload -Uz compinit && compinit"

completionsDirPath :: FilePath -> BKConfigPaths -> Shell -> FilePath
completionsDirPath homedir _ Fish = 
      homedir 
  </> ".config" 
  </> "fish" 
  </> "completions" 
completionsDirPath _ configPaths _ = 
  let configDir = bkConfigDirectoryPath configPaths       
   in configDir </> "completions"

initShellScript :: FilePath -> BKConfigPaths -> Shell -> Text
initShellScript homedir configPaths = _initShellScript
  where
    configDir = DT.pack $ bkConfigDirectoryPath configPaths     

    configDirEnvar :: (Text,Text)
    configDirEnvar = ("BK_CONFIG_DIR", configDir)

    _initShellScript :: Shell -> Text
    _initShellScript sh@Bash = exportEnvar sh configDirEnvar <> newline
                            <> sourceScript (DT.pack $ completionsDirPath homedir configPaths sh </> "_bk.sh")
    _initShellScript sh@Zsh  = exportEnvar sh configDirEnvar <> newline
                            <> updateZshFPath (completionsDirPath homedir configPaths sh) <> newline
                            <> compinitZsh
    _initShellScript _ = undefined

initShell :: Shell -> BKConfigPaths -> IO ()
initShell shell configPaths = undefined
