module Smos.Web.Style.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Path
import Path.IO
import Smos.Web.Style.Constants
import System.Environment
import System.Exit
import System.Process
import Yesod.EmbeddedStatic

mkStyle :: Q [Dec]
mkStyle = do
  md <- runIO $ lookupEnv "STYLE_FILE"
  embeddedStyle <- case md of
    Nothing -> do
      d <- resolveDir' "style"
      runIO $ putStrLn $ unwords ["WARNING: Including style files from dir at path: ", fromAbsDir d]
      cssFile <- resolveFile d "mybulma.css"
      pure $ embedFileAt "index.css" $ fromAbsFile cssFile
    Just f -> do
      runIO $ putStrLn $ unwords ["Including style in:", f]
      pure $ embedFileAt "index.css" f

  mkEmbeddedStatic
    development
    "smosWebStyle"
    [ embeddedStyle
    ]
