-- | Zero-mean normalized cross-correlation (template matching).
--
-- Correlates a template against a window of a larger-or-equal source image
-- placed at a given offset, using a single-pass Welford accumulation over the
-- three colour channels. The result is in @[-1, 1]@: @1@ is a perfect match,
-- @0@ means no linear relationship. The board parser searches a few offsets
-- and keeps the best score.
module Image.Zncc
  ( zncc
  , bestZncc
  ) where

import           Codec.Picture
import           Codec.Picture.Types

-- | ZNCC of @template@ against @source@ with the template's top-left corner at
-- @(offsetX, offsetY)@ in the source.
zncc :: Image PixelRGB8 -> Image PixelRGB8 -> (Int, Int) -> Double
zncc template source (offsetX, offsetY)
  | fits = correlation (pixelFold accumulate emptyStats template)
  | otherwise = 0
  where
    tw = imageWidth template
    th = imageHeight template
    sw = imageWidth source
    sh = imageHeight source
    fits =
      tw > 0
        && th > 0
        && offsetX >= 0
        && offsetY >= 0
        && tw <= sw
        && th <= sh
        && offsetX <= sw - tw
        && offsetY <= sh - th
    accumulate stats x y templatePixel =
      updateStats
        stats
        (pixelToC3 templatePixel)
        (pixelToC3 (pixelAt source (x + offsetX) (y + offsetY)))

-- | Best ZNCC of @template@ against @source@ over every offset within
-- @radius@ cells of @(centerX, centerY)@ (the template's nominal corner). Used
-- to tolerate small grid-alignment error when classifying a cell.
bestZncc :: Image PixelRGB8 -> Image PixelRGB8 -> Int -> (Int, Int) -> Double
bestZncc template source radius (centerX, centerY) =
  maximum
    ( 0 : [ zncc template source (centerX + dx, centerY + dy)
          | dy <- [-radius .. radius]
          , dx <- [-radius .. radius]
          ]
    )

-- | Welford running statistics over paired template/source pixels (per channel).
data ZNCCStats = ZNCCStats
  {-# UNPACK #-} !Int     -- ^ Number of accumulated pixels.
  !C3                     -- ^ Mean of template pixels.
  !C3                     -- ^ Mean of source pixels.
  {-# UNPACK #-} !Double  -- ^ Covariance between template and source pixels.
  {-# UNPACK #-} !Double  -- ^ Sum of squares of template pixel deviations.
  {-# UNPACK #-} !Double  -- ^ Sum of squares of source pixel deviations.

emptyStats :: ZNCCStats
emptyStats = ZNCCStats 0 c3Zero c3Zero 0 0 0

updateStats :: ZNCCStats -> C3 -> C3 -> ZNCCStats
updateStats (ZNCCStats count tMean sMean cov tM2 sM2) tPix sPix =
  ZNCCStats count' tMean' sMean' cov' tM2' sM2'
  where
    count' = count + 1
    scale  = recip (fromIntegral count')
    tDelta = c3Sub tPix tMean
    sDelta = c3Sub sPix sMean
    tMean' = c3Add tMean (c3Scale scale tDelta)
    sMean' = c3Add sMean (c3Scale scale sDelta)
    cov' = cov + c3Dot tDelta (c3Sub sPix sMean')
    tM2' = tM2 + c3Dot tDelta (c3Sub tPix tMean')
    sM2' = sM2 + c3Dot sDelta (c3Sub sPix sMean')

correlation :: ZNCCStats -> Double
correlation (ZNCCStats _ _ _ cov tM2 sM2)
  | tM2 <= 0 || sM2 <= 0 = 0
  | otherwise = max (-1) (min 1 (cov / sqrt (tM2 * sM2)))

-- A triple of channel values as doubles.
data C3 = C3 {-# UNPACK #-} !Double {-# UNPACK #-} !Double {-# UNPACK #-} !Double

c3Zero :: C3
c3Zero = C3 0 0 0

c3Add :: C3 -> C3 -> C3
c3Add (C3 a1 a2 a3) (C3 b1 b2 b3) = C3 (a1 + b1) (a2 + b2) (a3 + b3)

c3Sub :: C3 -> C3 -> C3
c3Sub (C3 a1 a2 a3) (C3 b1 b2 b3) = C3 (a1 - b1) (a2 - b2) (a3 - b3)

c3Scale :: Double -> C3 -> C3
c3Scale k (C3 a1 a2 a3) = C3 (k * a1) (k * a2) (k * a3)

c3Dot :: C3 -> C3 -> Double
c3Dot (C3 a1 a2 a3) (C3 b1 b2 b3) = a1 * b1 + a2 * b2 + a3 * b3

pixelToC3 :: PixelRGB8 -> C3
pixelToC3 (PixelRGB8 r g b) = C3 (fi r) (fi g) (fi b)
  where fi = fromIntegral @_ @Double
