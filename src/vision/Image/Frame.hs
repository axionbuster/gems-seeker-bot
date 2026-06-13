-- | Frame format conversions for captured screenshots.
--
-- TODO: port from references/experiments (@src/image/Image/Frame.hs@).
module Image.Frame
  ( pngToJpeg
  , placeholderJpeg
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

-- | Re-encode PNG bytes as JPEG bytes.
pngToJpeg :: ByteString -> ByteString
pngToJpeg = id

-- | A trivial placeholder frame, for wiring before real capture exists.
placeholderJpeg :: ByteString
placeholderJpeg = BS.empty
