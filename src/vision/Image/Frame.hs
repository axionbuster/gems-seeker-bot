-- | Frame resizing helpers.
module Image.Frame
  ( resizeNearest
  ) where

import           Codec.Picture

-- | Resize an RGB image with nearest-neighbour sampling. Pixel-art game assets
-- retain their hard edges, which keeps template masks stable across capture
-- scale factors.
resizeNearest :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
resizeNearest targetWidth targetHeight source =
  generateImage sample (max 1 targetWidth) (max 1 targetHeight)
  where
    sourceWidth  = imageWidth source
    sourceHeight = imageHeight source
    sample x y =
      pixelAt
        source
        (min (sourceWidth  - 1) (x * sourceWidth  `div` max 1 targetWidth ))
        (min (sourceHeight - 1) (y * sourceHeight `div` max 1 targetHeight))
