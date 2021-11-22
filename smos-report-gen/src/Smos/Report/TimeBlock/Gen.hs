{-# OPTIONS_GHC -fno-warn-orphans #-}

module Smos.Report.TimeBlock.Gen where

import Data.GenValidity
import Smos.Report.TimeBlock

instance GenValid TimeBlock where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering

instance (GenValid a, GenValid b) => GenValid (Block a b) where
  genValid = genValidStructurallyWithoutExtraChecking
  shrinkValid = shrinkValidStructurallyWithoutExtraFiltering
