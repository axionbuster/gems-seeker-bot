-- | Frame conversion helpers.
--
-- Ported from @references/experiments@ (@src/image/Image/Frame.hs@). MJPEG
-- streaming itself is out of scope (see CLAUDE.md), but PNG decode + JPEG
-- re-encode is handy for dumping debug frames, so the small surface is kept.
module Image.Frame
  ( pngToJpeg
  , placeholderJpeg
  ) where

import Codec.Picture
import Codec.Picture.Types (convertImage)
import Data.ByteString qualified as B
import Data.ByteString.Lazy qualified as BL

-- | Decode arbitrary image bytes (a PNG from @screencapture@ in practice) and
-- re-encode as JPEG. 'Left' carries the decoder's message on failure.
pngToJpeg :: B.ByteString -> Either String B.ByteString
pngToJpeg bytes = encodeRgb . convertRGB8 <$> decodeImage bytes

-- | A small black JPEG, useful as a placeholder.
placeholderJpeg :: B.ByteString
placeholderJpeg = encodeRgb (generateImage (\_ _ -> PixelRGB8 0 0 0) 16 16)

encodeRgb :: Image PixelRGB8 -> B.ByteString
encodeRgb img =
  BL.toStrict (encodeJpegAtQuality 80 (convertImage img :: Image PixelYCbCr8))
