{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
module BK.Setup 
    (setup) where

-- | * External Imports

import Data.Text              (Text)
import Control.Monad          (when)
import Control.Monad.IO.Class (MonadIO(..))
import System.FilePath        ((</>)
                              ,takeFileName
                              ,splitFileName)
import System.Exit            (exitFailure)
import GHC.Generics           (Generic)
import Toml.Schema            ((.=))

import Data.Text                qualified as DT
import Data.Text.IO             qualified as DT
import System.Environment       qualified as Env
import System.Console.Haskeline qualified as HL
import Toml                     qualified as Toml
import Toml.Schema              qualified as Toml

-- | * Internal Imports
import BK.BKMap qualified as BK
import BK.Lib   qualified as Lib
import Data.Bool (bool)

-- * Config File Parser

-- | Debugging configuration
data DebugConfig = DebugConfig {
  confDebugWorkDirectory :: Maybe FilePath -- ^ Location of testing programming data
} deriving (Eq, Show, Generic)
  deriving (Toml.ToValue) via Toml.GenericTomlTable DebugConfig

-- | BK config file format
data ConfigFile = ConfigFile {
   confDebug         :: Maybe DebugConfig  -- ^ Hidden debugging options
  ,confWorkDirectory :: FilePath           -- ^ Location to store program data
} deriving (Eq, Show, Generic)
  deriving (Toml.ToValue) via Toml.GenericTomlTable ConfigFile

instance Toml.FromValue DebugConfig where
  fromValue :: Toml.Value' l -> Toml.Matcher l DebugConfig
  fromValue = Toml.parseTableFromValue (DebugConfig <$> Toml.optKey "work-directory")

instance Toml.FromValue ConfigFile where
  fromValue :: Toml.Value' l -> Toml.Matcher l ConfigFile
  fromValue = Toml.parseTableFromValue (ConfigFile <$> Toml.optKey "debug"
                                                   <*> Toml.reqKey "work-directory")

instance Toml.ToTable ConfigFile where
  toTable :: ConfigFile -> Toml.Table
  toTable (ConfigFile Nothing workDir) = Toml.table ["work-directory" .= workDir]
  toTable (ConfigFile (Just debugConf) workDir) = Toml.table ["debug" .= debugConf, "work-directory" .= workDir]

parseConfigFile :: IO (Either Text ConfigFile)
parseConfigFile = do
  homeDir <- Lib.getHomeDirectory
  let pathToConfigFile = homeDir </> ".bk.toml"  
  (Lib.doesFileExist pathToConfigFile) >>= do
    bool (Lib.left $ errorMsg pathToConfigFile) $
        do configFileT <- DT.readFile $ pathToConfigFile
           let configFile' = Toml.decode configFileT :: Toml.Result String ConfigFile
           Lib.tomlResult configFile' 
            (Lib.left . Lib.concatErrors) 
            (\_ -> Lib.right)
  where 
    errorMsg pathToConfigFile = "Error: no such file or directory "
                             <> singleQuote (DT.pack pathToConfigFile)

-- | * Setup

-- | Set to @True@ to turn on debug logging.
_debugSetup :: Bool
_debugSetup = False

-- | Setup BK.
-- Discovers the BK configuration, but if this is the first time running BK,
-- then we enter an interactive setup process. 
-- Nonrecoverable errors exit with failure.
--
                                    
setup :: IO BKConfig
setup = do
  -- Parse the config file:     
    either 
      (\_ -> interactiveSetup Nothing)
      continueSetup
    =<< parseConfigFile

-- | Supported configuration files.
-- These are all stored under the @$CONFIG_DIR/bk@ directory.
data BKConfig = BKConfig {
   -- | Path to the configuration directory
   bkConfigDirectoryPath :: FilePath
   -- | Path to the booksmarks CSV file.
  ,bkBookmarksFile :: FilePath
} deriving Show

-- | Returns the absolute paths to the config files.
bkConfigPaths 
  :: FilePath          -- ^ Path to the configuration directory
  -> BKConfig
bkConfigPaths configDir = BKConfig {
   bkConfigDirectoryPath = configDir
  ,bkBookmarksFile       = configDir </> "bookmarks.csv"
}

-- | Convenience function for building the path to the bookmarks CSV file.
getBKBookmarksFilePath 
  :: FilePath -- ^ Path to the configuration directory
  -> FilePath
getBKBookmarksFilePath = bkBookmarksFile . bkConfigPaths

-- | Tests whether all of the configuration files exist in the configuration directory.
doesProgramDataExist 
  :: FilePath                       -- ^ Path to the configuration directory
  -> IO (Either Text BKConfig) -- ^ Absolute paths to all of the configuration files.
doesProgramDataExist configDir = do
  d <- Lib.doesDirectoryExist configDir  
  if d
  then do let configFilesPaths = BKConfig { 
                 bkConfigDirectoryPath = configDir
                ,bkBookmarksFile       = (getBKBookmarksFilePath configDir) 
              }
          f2 <- Lib.doesFileExist $ bkBookmarksFile configFilesPaths
          return $ if f2
                   then Right $ configFilesPaths
                   else Left "missing CSV file"
  else return . Left $ "no such file or directory '"<>DT.pack configDir<>"'"

getWorkDir :: ConfigFile -> FilePath
getWorkDir (ConfigFile (Just (DebugConfig (Just wd))) _) = wd
getWorkDir (ConfigFile _ wd) = wd

-- | Continues the setup to either an interactive setup if this is the first
-- time running @bk@ and initializing the configuration directory, or just
-- initializing the configuration paths.
continueSetup 
  :: ConfigFile   -- ^ Configuration file contents
  -> IO BKConfig
continueSetup configFile = 
        doesProgramDataExist (getWorkDir configFile) 
          >>= either goInteractive return  
  where
    goInteractive errMsg = do
      when _debugSetup (Lib.putStrLnStdErr errMsg)
      interactiveSetup (Just configFile)

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
        let shellEnvar = splitFileName $ maybe "" id osShellM
        currentShell <- liftIO $ getCurrentShell shellEnvar
        let defaultValue = DT.pack $ fst shellEnvar </> snd shellEnvar
        let prompt = DT.unpack $ prefix <> space <> squareBracket "bzf" <> questionMark <> space
        _promptForShell prompt defaultValue currentShell
  where     
    getCurrentShell :: (String,String) -> IO Shell
    getCurrentShell shellEnvar =
      maybe (exitFailureWithMsg "Aborting setup!\nUnable to determine shell")
            return 
            $ parseShellEnvar (snd shellEnvar)

    _promptForShell prompt defaultValue currentShell = do
        liftIO $ Lib.putStrLnStdOut $ 
                 newline 
               <> "Shell configuration"<>newline
               <> "Supported shells: [b]ash, [z]sh, and [f]ish" <> newline
               <> "Your current shell is "<>defaultValue
        HL.getInputLine prompt
          >>= maybe (_promptForShell prompt defaultValue currentShell)
                (\sh -> case sh of
                          "" -> return currentShell
                          "b" -> return Bash
                          "z" -> return Zsh
                          "f" -> return Fish
                          _ -> _promptForShell prompt defaultValue currentShell)

parseShellEnvar :: String -> Maybe Shell
parseShellEnvar (takeFileName->"zsh")  = Just Zsh
parseShellEnvar (takeFileName->"bash") = Just Bash
parseShellEnvar (takeFileName->"fish") = Just Fish
parseShellEnvar _                      = Nothing

-- | Interactive setup that configures @bk@ for the first time.
interactiveSetup :: Maybe ConfigFile -> IO BKConfig
interactiveSetup configFileM = HL.runInputT HL.defaultSettings $ loop 
  where     
    loop = do
      homedir <- liftIO Lib.getHomeDirectory
      let configFilePath = homedir </> ".bk.toml"
      liftIO $ Lib.putStrLnStdOut $ "Welcome to the Beekeeper (bk) Interactive Setup!\n"
      promptYesNo 
        "Is this the first time running bk"
        (liftIO . exitFailureWithMsg $ secondRunMessage (configFileM,configFilePath)) $
          do liftIO $ Lib.putStrLnStdOut newline
             -- Split this based on if ConfigFileM is Nothing or Just. 
             -- 
             -- If Nothing, then prompt as follows and create a fresh configFile
             -- and set bkConfigPaths according.
             -- 
             -- If Just _, then skip the prompt and report that we are using the
             -- path from the configuration file. Then set bkConfigPaths based
             -- on the config file. 
             --
             -- Maybe create a promptForConfigFile that returns a BKConfig
             -- based on the above reasoning. I think this would be cleaner.
             configDirParent <- promptForFilePath 
                                  "Enter the directory where you would like to store bk's program data?" 
                                  homedir             
             let configFilePaths = bkConfigPaths $ configDirParent 
                               </> if configDirParent == homedir
                                   then ".bk"
                                   else "bk"
             shell <- promptForShell "Which shell should be initialized"
             --liftIO $ initializeConfigDir configFilePaths shell
             return configFilePaths
  
    secondRunMessage :: (Maybe ConfigFile,FilePath) -> Text
    secondRunMessage (Nothing,configFilePath) = 
         "Since you are seeing this setup for the second time, then you most likely\n"
      <> "setup a custom location to store the configuration files in, and bk can no\n"
      <> "longer find that location.\n\n"
      <> "Please check that the configuration file "<>singleQuote (DT.pack configFilePath)
      <> " exists."
    secondRunMessage (Just configFile,configFilePath) = 
         "Since you are seeing this setup for the second time, then bk can no longer\n"
      <> "find the directory "<>singleQuote (DT.pack $ getWorkDir configFile)
      <> ", because the configuration file "<>singleQuote (DT.pack configFilePath)
      <> " exists."

initializeConfigDir :: BKConfig -> Shell -> IO ()
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

completionsDirPath :: FilePath -> BKConfig -> Shell -> FilePath
completionsDirPath homedir _ Fish = 
      homedir 
  </> ".config" 
  </> "fish" 
  </> "completions" 
completionsDirPath _ configPaths _ = 
  let configDir = bkConfigDirectoryPath configPaths       
   in configDir </> "completions"

initShellScript :: FilePath -> BKConfig -> Shell -> Text
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

initShell :: Shell -> BKConfig -> IO ()
initShell shell configPaths = undefined
