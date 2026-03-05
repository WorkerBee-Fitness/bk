{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
     modes, auto, typ, Mode)
import System.Environment (withArgs)
import Control.Monad (void)
import System.Console.CmdArgs (CmdArgs)

data BKMode 
    = Default {
            find    :: Maybe String,
            remove  :: Maybe String,
            run     :: Maybe String
        } 
    | Add {
            label :: Maybe String,
            cmd   :: Maybe String
        }
    deriving (Show, Data, Typeable)

data AddOption = AddOption  deriving (Show, Data, Typeable)

addOption :: BKMode
addOption = Add {
    label = def &= help "the label to add",
    cmd = def &= help "the command to add"
}

option :: BKMode
option = Default {       
    find   = def &= help "find a bookmark",
    remove = def &= help "remove a bookmark",
    run    = def &= args
} &= auto

mainModes :: Mode  (CmdArgs BKMode)
mainModes = cmdArgsMode $ modes [option, addOption] 
    &= summary "bk 0.0.0.1"

handleAddbk :: String -> String -> IO ()
handleAddbk label cmd = undefined

handleFindbk :: String -> IO ()
handleFindbk label = undefined

handleRemovebk :: String -> IO ()
handleRemovebk label = undefined

handleRunbk :: String -> IO ()
handleRunbk label = undefined

mainLoop ::  IO ()
mainLoop = do
    opts <- cmdArgsRun mainModes 
    case opts of
        Add Nothing _ -> error "error: add: [--label] required"
        Add _ Nothing -> error "error: add: [--cmd] required"
        Add (Just l) (Just c) -> handleAddbk l c
        Default (Just l) _ _  -> handleFindbk l
        Default _ (Just l) _  -> handleRemovebk l
        Default _ _ (Just l)  -> handleRunbk l
        Default _ _ Nothing   -> 
            void $ withArgs ["--help"] $ cmdArgsRun mainModes
