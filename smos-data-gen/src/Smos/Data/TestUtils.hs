{-# LANGUAGE LambdaCase #-}

module Smos.Data.TestUtils where

import Control.Monad
import Path.IO
import Smos.Data
import Test.Syd

pureGoldenSmosFile :: FilePath -> SmosFile -> GoldenTest SmosFile
pureGoldenSmosFile path = goldenSmosFile path . pure

goldenSmosFile :: FilePath -> IO SmosFile -> GoldenTest SmosFile
goldenSmosFile path produceSmosFile =
  GoldenTest
    { goldenTestRead = do
        absFile <- resolveFile' path
        mErrOrRes <- readSmosFile absFile
        forM mErrOrRes $ \case
          Left err -> expectationFailure err
          Right smosFile -> pure smosFile,
      goldenTestProduce = produceSmosFile,
      goldenTestWrite = \smosFile -> do
        absFile <- resolveFile' path
        writeSmosFile absFile smosFile,
      goldenTestCompare = \actual expected ->
        if actual == expected
          then Nothing
          else
            Just $
              Context
                ( bytestringsNotEqualButShouldHaveBeenEqual
                    (smosFileBS actual)
                    (smosFileBS expected)
                )
                (goldenContext path)
    }
