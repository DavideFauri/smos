{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Server.OptParse.Types where

import Autodocodec
import Control.Monad.Logger
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import Data.Time
import Data.Word
import GHC.Generics (Generic)
import Looper
import Path
import Smos.API
import Text.Read

data Flags = Flags
  { flagConfigFile :: !(Maybe FilePath),
    flagLogLevel :: !(Maybe LogLevel),
    flagUUIDFile :: !(Maybe FilePath),
    flagDatabaseFile :: !(Maybe FilePath),
    flagSigningKeyFile :: !(Maybe FilePath),
    flagPort :: !(Maybe Int),
    flagMaxBackupSizePerUser :: !(Maybe Word64),
    flagAutoBackupLooperFlags :: !LooperFlags,
    flagBackupGarbageCollectionLooperFlags :: !LooperFlags,
    flagFileMigrationLooperFlags :: !LooperFlags,
    flagAdmin :: !(Maybe Username)
  }
  deriving (Show, Eq, Generic)

data Environment = Environment
  { envConfigFile :: !(Maybe FilePath),
    envLogLevel :: !(Maybe LogLevel),
    envUUIDFile :: !(Maybe FilePath),
    envDatabaseFile :: !(Maybe FilePath),
    envSigningKeyFile :: !(Maybe FilePath),
    envPort :: !(Maybe Int),
    envMaxBackupSizePerUser :: !(Maybe Word64),
    envAutoBackupLooperEnv :: !LooperEnvironment,
    envBackupGarbageCollectionLooperEnv :: !LooperEnvironment,
    envFileMigrationLooperEnv :: !LooperEnvironment,
    envAdmin :: !(Maybe Username)
  }
  deriving (Show, Eq, Generic)

data Configuration = Configuration
  { confLogLevel :: !(Maybe LogLevel),
    confUUIDFile :: !(Maybe FilePath),
    confDatabaseFile :: !(Maybe FilePath),
    confSigningKeyFile :: !(Maybe FilePath),
    confPort :: !(Maybe Int),
    confMaxBackupsPerPeriodPerUser :: !(Maybe [(NominalDiffTime, Word)]),
    confMaxBackupSizePerUser :: !(Maybe Word64),
    confAutoBackupLooperConfiguration :: !(Maybe LooperConfiguration),
    confBackupGarbageCollectionLooperConfiguration :: !(Maybe LooperConfiguration),
    confFileMigrationLooperConfiguration :: !(Maybe LooperConfiguration),
    confAdmin :: !(Maybe Username)
  }
  deriving stock (Show, Eq, Generic)

instance HasCodec Configuration where
  codec = object "Configuration" configurationObjectCodec

configurationObjectCodec :: JSONObjectCodec Configuration
configurationObjectCodec =
  Configuration
    <$> optionalFieldOrNullWith
      "log-level"
      (bimapCodec parseLogLevel renderLogLevel codec)
      "The minimal severity for log messages"
      .= confLogLevel
    <*> optionalFieldOrNull
      "uuid-file"
      "The file in which to store the server uuid"
      .= confUUIDFile
    <*> optionalFieldOrNull
      "database-file"
      "The file in which to store the database"
      .= confDatabaseFile
    <*> optionalFieldOrNull
      "signing-key-file"
      "The file in which to store signing key for JWT tokens"
      .= confSigningKeyFile
    <*> optionalFieldOrNull
      "port"
      "The port on which to serve api requests"
      .= confPort
    <*> optionalFieldOrNullWith
      "max-backups-per-user-per-period"
      ( singleOrListCodec $
          object "Period" $
            (,)
              <$> requiredField "period" "period, in seconds" .= fst
              <*> requiredField "max-backups" "maximum backups in this period" .= snd
      )
      "The maximum number of backups per user per period"
      .= confMaxBackupsPerPeriodPerUser
    <*> optionalFieldOrNull
      "max-backup-size-per-user"
      "The maximum number of bytes that backups can take up per user"
      .= confMaxBackupSizePerUser
    <*> optionalFieldOrNull
      "auto-backup"
      "The configuration for the automatic backup looper"
      .= confAutoBackupLooperConfiguration
    <*> optionalFieldOrNull
      "backup-garbage-collector"
      "The configuration for the automatic backup garbage collection looper"
      .= confBackupGarbageCollectionLooperConfiguration
    <*> optionalFieldOrNull
      "file-migrator"
      "The configuration for the automatic file format migrator looper"
      .= confFileMigrationLooperConfiguration
    <*> optionalFieldOrNull
      "admin"
      "The username of the user who will have admin rights"
      .= confAdmin

data Settings = Settings
  { settingLogLevel :: !LogLevel,
    settingUUIDFile :: !(Path Abs File),
    settingDatabaseFile :: !(Path Abs File),
    settingSigningKeyFile :: !(Path Abs File),
    settingPort :: !Int,
    settingMaxBackupsPerPeriodPerUser :: ![(NominalDiffTime, Word)],
    settingMaxBackupSizePerUser :: !(Maybe Word64),
    settingAutoBackupLooperSettings :: !LooperSettings,
    settingBackupGarbageCollectionLooperSettings :: !LooperSettings,
    settingFileMigrationLooperSettings :: !LooperSettings,
    settingAdmin :: !(Maybe Username)
  }
  deriving (Show, Eq, Generic)


parseLogLevel :: String -> Either String LogLevel
parseLogLevel s = case readMaybe $ "Level" <> s of
  Nothing -> Left $ unwords ["Unknown log level: " <> show s]
  Just ll -> Right ll

renderLogLevel :: LogLevel -> String
renderLogLevel = drop 5 . show
