-- | Recognize non-board screens that the bot can advance automatically.
module Vision.Screen
  ( findPlayButton
  ) where

import           Codec.Picture (Image, PixelRGB8, imageHeight, imageWidth)
import           Image.Frame   (resizeNearest)
import           Image.Zncc    (zncc)

-- | Locate the centre of the pixel-font @PLAY@ label near the bottom of the
-- mirrored phone image. The supplied template is a tight crop of the word.
findPlayButton :: Image PixelRGB8 -> Image PixelRGB8 -> Maybe (Int, Int)
findPlayButton sourceTemplate image
  | imageWidth image < templateWidth || imageHeight image < templateHeight = Nothing
  | bestScore < 0.8 = Nothing
  | otherwise = Just (bestX + templateWidth `div` 2, bestY + templateHeight `div` 2)
  where
    baseWidth :: Double
    baseWidth = 696
    scale = fromIntegral (imageWidth image) / baseWidth
    template =
      resizeNearest
        (round (fromIntegral (imageWidth sourceTemplate) * scale))
        (round (fromIntegral (imageHeight sourceTemplate) * scale))
        sourceTemplate
    templateWidth = imageWidth template
    templateHeight = imageHeight template
    expectedX = (imageWidth image - templateWidth) `div` 2
    expectedY = round (0.865 * fromIntegral (imageHeight image) :: Double)
    candidates =
      [ (zncc template image (x, y), x, y)
      | offsetY <- [-12 .. 12]
      , offsetX <- [-12 .. 12]
      , let x = expectedX + offsetX
      , let y = expectedY + offsetY
      ]
    (bestScore, bestX, bestY) = maximum candidates
