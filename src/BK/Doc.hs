module BK.Doc () where

import Hikchr qualified as Pikchr
import qualified Data.Text.IO as DT
import System.FilePath ((</>))
import qualified BK.Lib as Lib

genSetupDiagram :: IO ()
genSetupDiagram = do
    let assestsDir = "doc" </> "setup" </> "assets"
    script <- DT.readFile $ assestsDir </> "setup-diagram.pikchr"
    either 
        Lib.exitFailureWithMsg
        (DT.writeFile $ assestsDir </> "setup-diagram.svg")
      =<< Pikchr.hikchrCustom
      ( Pikchr.HikchrConfig
          { Pikchr.svgClass = Just "setup diagram"
          , Pikchr.darkMode = False
          , Pikchr.width = Nothing
          , Pikchr.height = Nothing
          }
      ) script
    
