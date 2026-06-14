-- | Frame conversion helpers.
--
-- PNG decode plus JPEG re-encode is handy for dumping debug frames, so the
-- small surface is kept even though the app does not stream MJPEG.
module Image.Frame
  ( pngToJpeg
  , placeholderJpeg
  , resizeNearest
  ) where

import           Codec.Picture
import           Codec.Picture.Types  (convertImage)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL

-- | Decode arbitrary image bytes (a PNG from @screencapture@ in practice) and
-- re-encode as JPEG. 'Left' carries the decoder's message on failure.
pngToJpeg :: B.ByteString -> Either String B.ByteString
pngToJpeg bytes = encodeRgb . convertRGB8 <$> decodeImage bytes

-- | A small black JPEG, useful as a placeholder.
placeholderJpeg :: B.ByteString
placeholderJpeg = encodeRgb (generateImage (\_ _ -> PixelRGB8 0 0 0) 16 16)

-- | Resize an RGB image with nearest-neighbour sampling. Pixel-art game assets
-- retain their hard edges, which keeps template masks stable across capture
-- scale factors.
resizeNearest :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
resizeNearest targetWidth targetHeight source =
  generateImage sample (max 1 targetWidth) (max 1 targetHeight)
  where
    sourceWidth = imageWidth source
    sourceHeight = imageHeight source
    sample x y =
      pixelAt
        source
        (min (sourceWidth - 1) (x * sourceWidth `div` max 1 targetWidth))
        (min (sourceHeight - 1) (y * sourceHeight `div` max 1 targetHeight))

encodeRgb :: Image PixelRGB8 -> B.ByteString
encodeRgb img =
  BL.toStrict (encodeJpegAtQuality 80 (convertImage img :: Image PixelYCbCr8))
