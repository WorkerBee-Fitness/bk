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
import GHC.Generics           (Generic)
import Toml.Schema            ((.=))

import Data.Text                qualified as DT
import Data.Text.IO             qualified as DT
import System.Environment       qualified as Env
import System.Console.Haskeline qualified as HL
import Toml                     qualified as Toml
import Toml.Schema              qualified as Toml
import Toml.Schema.FromValue    qualified as Toml

-- | * Internal Imports
import BK.BKMap qualified as BK
import BK.Lib   qualified as Lib
import Data.Bool (bool)

-- * Config File Parser

-- | BK config file format
data ConfigFile = ConfigFile {
    confWorkDirectory :: FilePath           -- ^ Location to store program data
   ,confShell         :: Shell              -- ^ Preferred shell
} deriving (Eq, Show, Generic)
  deriving (Toml.ToValue) via Toml.GenericTomlTable ConfigFile

instance Toml.FromValue Shell where
  fromValue :: Toml.Value' l -> Toml.Matcher l Shell
  fromValue (Toml.Text' _ "zsh")  = pure Zsh
  fromValue (Toml.Text' _ "bash") = pure Bash
  fromValue (Toml.Text' _ "fish") = pure Fish
  fromValue v = Toml.typeError "expected a shell" v

instance Toml.FromValue ConfigFile where
  fromValue :: Toml.Value' l -> Toml.Matcher l ConfigFile
  fromValue = Toml.parseTableFromValue (ConfigFile <$> Toml.reqKey "work-directory"
                                                   <*> Toml.reqKey "shell")

instance Toml.ToTable ConfigFile where
  toTable :: ConfigFile -> Toml.Table
  toTable (ConfigFile workDir shell)  = Toml.table ["work-directory" .= workDir, "shell" .= shell]  

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
-- The complete setup flow is documented in `doc/setup/setup-diagram.svg`.
                                    
setup :: IO BKConfig
setup = do 
  bkConfig <- either 
                (\_ -> interactiveSetup Nothing)
                continueSetup
              =<< parseConfigFile    
  initializeProgramData bkConfig
  return bkConfig

data BKProgramDirFiles = BKProgramDirFiles {
   -- | Path to the program-data directory
   bkConfigDirectoryPath :: FilePath
   -- | Path to the bookmarks CSV file.
  ,bkConfigBookmarksFile :: FilePath
} deriving Show

-- | Supported configuration fields specified by the configuration file.
data BKConfig = BKConfig {
  bkProgramDirFiles      :: BKProgramDirFiles
  -- | Preferred shell
  ,bkConfigShell         :: Shell
} deriving Show

-- | Returns the absolute paths to the config files.
bkConfigPaths 
  :: FilePath          -- ^ Path to the program-data directory
  -> BKProgramDirFiles
bkConfigPaths configDir = BKProgramDirFiles {
   bkConfigDirectoryPath  = configDir
  ,bkConfigBookmarksFile  = configDir </> "bookmarks.csv"
}

-- | Convenience function for building the path to the bookmarks CSV file.
getBKBookmarksFilePath 
  :: FilePath -- ^ Path to the program-data directory
  -> FilePath
getBKBookmarksFilePath = bkConfigBookmarksFile . bkConfigPaths

-- | Tests whether all of the configuration files exist in the program-data directory.
doesProgramDataExist 
  :: FilePath                       -- ^ Path to the program-data directory
  -> IO (Either Text BKProgramDirFiles)      -- ^ Absolute paths to all of the configuration files.
doesProgramDataExist configDir = do
  d <- Lib.doesDirectoryExist configDir  
  if d
  then do let bkConfig = BKProgramDirFiles { 
                 bkConfigDirectoryPath = configDir
                ,bkConfigBookmarksFile       = (getBKBookmarksFilePath configDir) 
              }
          f2 <- Lib.doesFileExist $ bkConfigBookmarksFile bkConfig
          return $ if f2
                   then Right $ bkConfig
                   else Left "missing CSV file"
  else return . Left $ "no such file or directory '"<>DT.pack configDir<>"'"

-- | Returns the program data directory from the configuration file.
getWorkDir 
  :: ConfigFile -- ^ Config file
  -> FilePath
getWorkDir (ConfigFile wd _) = wd

-- | Continues the setup to either an interactive setup if this is the first
-- time running @bk@ and initializing the program-data directory, or just
-- initializing the configuration paths.
continueSetup 
  :: ConfigFile   -- ^ Configuration file contents
  -> IO BKConfig
continueSetup configFile = 
        doesProgramDataExist (getWorkDir configFile) 
          >>= either goInteractive 
                     (return . flip BKConfig (confShell configFile))
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
      maybe (Lib.exitFailureWithMsg "Aborting setup!\nUnable to determine shell")
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

promptForConfigFile :: Maybe ConfigFile -> HL.InputT IO BKConfig
promptForConfigFile (Just (ConfigFile wd shell)) = do
  liftIO . Lib.putStrLnStdOut $ "Found configuration file.\n"
                             <> "Saving all program data to the path "<>singleQuote (DT.pack wd)<>"\n"
  return $ BKConfig { 
       bkProgramDirFiles = bkConfigPaths wd
      ,bkConfigShell     = shell 
    }
promptForConfigFile Nothing = do  
  homedir <- liftIO $ Lib.getHomeDirectory
  configDirParent <- promptForFilePath 
                       "Enter the directory where you would like to store bk's program data?" 
                       homedir             
  let _bkProgramDirFiles = bkConfigPaths $ configDirParent 
                       </> if configDirParent == homedir
                           then ".bk"
                           else "bk"
  shell <- promptForShell "Which shell should be initialized"
  return $ BKConfig { 
       bkProgramDirFiles = _bkProgramDirFiles
      ,bkConfigShell     = shell 
    } 

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
        (liftIO . Lib.exitFailureWithMsg $ secondRunMessage (configFileM,configFilePath)) $
          do liftIO $ Lib.putStrLnStdOut newline
             bkConfig <- promptForConfigFile configFileM
             return bkConfig
  
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

initializeProgramData :: BKConfig -> IO ()
initializeProgramData (BKConfig programDirFiles@(BKProgramDirFiles configDir csvFile) shell) = do 
    b <- Lib.doesFileExist csvFile
    when b $ do Lib.exitFailureWithMsg $ "Aborting setup!\n"
                                      <> "It seems bk is already setup, because " 
                                      <> DT.pack csvFile
                                      <> " already exists."
    Lib.createDirectoryIfMissing False configDir
    createCompletionsDir shell 
    BK.writeCSVFile csvFile BK.emptyBKMap
  where
    createCompletionsDir Fish = return ()
    createCompletionsDir _sh = do
      homedir <- Lib.getHomeDirectory
      let path = completionsDirPath homedir programDirFiles _sh
      Lib.createDirectoryIfMissing False path

-- | Supported shell initialization.
data Shell = Zsh 
           | Bash 
           | Fish
  deriving (Eq, Generic)

instance Toml.ToValue Shell where
  toValue :: Shell -> Toml.Value
  toValue = Toml.toValue . show

instance Show Shell where
  show :: Shell -> String
  show Zsh  = "zsh"
  show Bash = "bash"
  show Fish = "fish"

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

_updatePath :: Shell -> Text -> Text
_updatePath Fish dir = "fish_add_path "<>doubleQuote dir
_updatePath sh   dir = exportEnvar sh ("PATH",doubleQuote $ dir<>colon<>"$PATH")

sourceScript :: Text -> Text
sourceScript script = "source " <> script

updateZshFPath :: FilePath -> Text
updateZshFPath (DT.pack->dir) = "fpath=("<>doubleQuote dir<>" $fpath)"

compinitZsh :: Text
compinitZsh = "autoload -Uz compinit && compinit"

completionsDirPath :: FilePath -> BKProgramDirFiles -> Shell -> FilePath
completionsDirPath homedir _ Fish = 
      homedir 
  </> ".config" 
  </> "fish" 
  </> "completions" 
completionsDirPath _ configPaths _ = 
  let configDir = bkConfigDirectoryPath configPaths       
   in configDir </> "completions"

_initShellScript :: FilePath -> BKProgramDirFiles -> Shell -> Text
_initShellScript homedir configPaths = _initShellScript
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


--initShell :: Shell -> BKConfig -> IO ()
--initShell _shell _configPaths = undefined
