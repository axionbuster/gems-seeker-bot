-- | macOS window geometry and frame grabbing.
--
-- Capture is driven externally; this only needs to grab the 1-3 frames the
-- vision pass reads. TODO: port from references/experiments
-- (@src/mac/Mac/Mirror.hs@): @osascript@ for geometry, @screencapture@ for
-- pixels.
module Mac.Mirror
  ( Rect (..)
  , findWindow
  , focusApp
  , captureFrame
  ) where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

-- | A screen rectangle, in pixels.
data Rect = Rect
  { rectX :: {-# UNPACK #-} !Int
  , rectY :: {-# UNPACK #-} !Int
  , rectW :: {-# UNPACK #-} !Int
  , rectH :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Locate a window by application name (e.g. iPhone Mirroring).
findWindow :: String -> IO (Maybe Rect)
findWindow _appName = pure Nothing

-- | Bring an application to the foreground.
focusApp :: String -> IO ()
focusApp _appName = pure ()

-- | Grab a screen region as PNG bytes.
captureFrame :: Rect -> IO ByteString
captureFrame _region = pure BS.empty
