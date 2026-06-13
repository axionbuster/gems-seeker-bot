-- | Recover a 'Board' from captured frames.
--
-- 1-3 good frames are enough to read the whole board; the solver then plays it
-- out completely, so this runs once at the start. Calibration is already done
-- (see below) — do not re-derive thresholds at runtime.
module Vision.Board
  ( Thresholds (..)
  , calibratedThresholds
  , parseBoard
  ) where

import Board (Board)
import Codec.Picture (DynamicImage)

-- | Per-cell classification thresholds, measured and frozen from the
-- calibration run in references/experiments (@calibration.txt@). These are
-- constants now.
data Thresholds = Thresholds
  { gemLumaThreshold :: {-# UNPACK #-} !Double
  , gemMaskThreshold :: {-# UNPACK #-} !Double
  , gemYellowFraction :: {-# UNPACK #-} !Double
  , batLumaThreshold :: {-# UNPACK #-} !Double
  , batMaskThreshold :: {-# UNPACK #-} !Double
  , batCyanFraction :: {-# UNPACK #-} !Double
  , airForegroundFraction :: {-# UNPACK #-} !Double
  , occupiedForegroundFraction :: {-# UNPACK #-} !Double
  }
  deriving (Eq, Show)

-- | The frozen calibration from references/experiments/calibration.txt.
calibratedThresholds :: Thresholds
calibratedThresholds =
  Thresholds
    { gemLumaThreshold = 0.818193412917773
    , gemMaskThreshold = 0.4897744778323243
    , gemYellowFraction = 5.0e-2
    , batLumaThreshold = 0.42380862933019015
    , batMaskThreshold = 0.3946005657078237
    , batCyanFraction = 0.1
    , airForegroundFraction = 7.719884503474203e-2
    , occupiedForegroundFraction = 0.19444444444444445
    }

-- | Detect the grid and classify each cell into a 'Board'.
--
-- NOTE: the saved grid origin in references/experiments is only valid for one
-- grid size; reusing it verbatim pushes cells negative on larger boards.
-- Derive origin and extent from the frame so any board fits — by design the
-- whole Gem Seeker board always fits on screen.
--
-- TODO: port grid detection + cell classification from
-- references/experiments (@app/Experiment2.hs@, @docs/superpowers/specs@).
parseBoard :: [DynamicImage] -> Either String Board
parseBoard _frames = Left "Vision.Board.parseBoard: not implemented"
