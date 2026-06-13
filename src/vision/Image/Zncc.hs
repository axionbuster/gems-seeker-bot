-- | Zero-mean normalized cross-correlation for template matching.
--
-- TODO: port from references/experiments (@src/image/Image/Zncc.hs@).
-- Performance matters here.
module Image.Zncc
  ( zncc
  ) where

import Codec.Picture (Image, PixelF)

-- | Correlate a template against an equally-sized patch. Range @[-1, 1]@; @1@
-- is a perfect match, @0@ no linear relationship.
zncc :: Image PixelF -> Image PixelF -> Double
zncc _template _patch = 0
