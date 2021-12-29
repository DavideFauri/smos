{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Single.OptParse
  ( module Smos.Single.OptParse,
    module Smos.Single.OptParse.Types,
  )
where

import Control.Monad
import qualified Data.Text as T
import Data.Version
import qualified Env
import Options.Applicative
import Options.Applicative.Help.Pretty as Doc
import Path
import Paths_smos_single
import Smos.Data
import qualified Smos.Report.Config as Report
import qualified Smos.Report.OptParse as Report
import Smos.Single.OptParse.Types
import qualified System.Environment as System
import System.Exit

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  env <- getEnvironment
  config <- getConfig flags env
  deriveSettings (Report.flagWithRestFlags flags) (Report.envWithRestEnv env) config

getConfig :: Report.FlagsWithConfigFile Flags -> Report.EnvWithConfigFile Environment -> IO (Maybe Configuration)
getConfig f e = fmap Configuration <$> Report.getConfiguration f e

deriveSettings :: Flags -> Environment -> Maybe Configuration -> IO Settings
deriveSettings Flags {..} Environment {..} mc = do
  setTask <-
    case parseHeader $ T.pack $ unwords flagTaskPieces of
      Left err -> die $ "Failed to parse header: " <> err
      Right h -> pure h
  setDirectorySettings <-
    Report.combineToDirectoryConfig
      Report.defaultDirectoryConfig
      flagDirectoryFlags
      envDirectoryEnvironment
      (confDirectoryConfiguration <$> mc)
  setTaskFile <- forM flagTaskFile parseRelFile
  pure Settings {..}

getFlags :: IO (Report.FlagsWithConfigFile Flags)
getFlags = do
  args <- System.getArgs
  let result = runArgumentsParser args
  handleParseResult result

runArgumentsParser :: [String] -> ParserResult (Report.FlagsWithConfigFile Flags)
runArgumentsParser = execParserPure prefs_ flagsParser
  where
    prefs_ =
      defaultPrefs
        { prefShowHelpOnError = True,
          prefShowHelpOnEmpty = True
        }

flagsParser :: ParserInfo (Report.FlagsWithConfigFile Flags)
flagsParser = info (helper <*> parseFlags) help_
  where
    help_ = fullDesc <> progDescDoc (Just description)
    description :: Doc
    description =
      Doc.vsep $
        map Doc.text $
          [ "",
            "Smos Single-task Tool version: " <> showVersion version,
            ""
          ]
            ++ writeDataVersionsHelpMessage

parseFlags :: Parser (Report.FlagsWithConfigFile Flags)
parseFlags =
  Report.parseFlagsWithConfigFile $
    Flags
      <$> some
        ( strArgument
            ( mconcat
                [ help "The task. Pass any number of arguments and they will be interpreted as the task together.",
                  metavar "TASK"
                ]
            )
        )
      <*> optional
        ( strOption
            ( mconcat
                [ long "file",
                  help "The file to put the task in",
                  metavar "FILEPATH"
                ]
            )
        )
      <*> Report.parseDirectoryFlags

getEnvironment :: IO (Report.EnvWithConfigFile Environment)
getEnvironment = Env.parse (Env.header "Environment") prefixedEnvironmentParser

prefixedEnvironmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
prefixedEnvironmentParser = Env.prefixed "SMOS_" environmentParser

environmentParser :: Env.Parser Env.Error (Report.EnvWithConfigFile Environment)
environmentParser = Report.envWithConfigFileParser $ Environment <$> Report.directoryEnvironmentParser
