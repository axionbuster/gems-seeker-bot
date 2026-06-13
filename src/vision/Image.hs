-- | Re-export surface for image work: JuicyPixels plus our own operations.
--
-- This is the "virtual dependency" pattern (see CLAUDE.md): import @Image@ and
-- get the pixel types alongside 'zncc' and the frame conversions, the way an
-- equivalent Python project would expose one image package.
module Image
  ( module Codec.Picture
  , module Image.Zncc
  , module Image.Frame
  ) where

import Codec.Picture
import Image.Frame
import Image.Zncc
