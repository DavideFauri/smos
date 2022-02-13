{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.Period.Gen where

import Data.GenValidity
import Data.GenValidity.Time ()
import Smos.Data.Gen ()
import Smos.Report.Period

instance GenValid Period where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
