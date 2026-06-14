-- | macOS control of the mirroring app: locate and focus its native window,
-- capture RGB pixels, and (via "Mac.Gesture") swipe it. Window geometry is in
-- screen points; captured image dimensions use native display pixels.
module Mac.Mirror
  ( Rect (..)
  , Window
  , windowRect
  , windowCenter
  , selectPhoneWindow
  , findWindow
  , focusApp
  , captureFrame
  ) where

import           Codec.Picture.Types (Image, PixelRGB8)
import           Data.List           (find, maximumBy)
import           Data.Ord            (comparing)
import qualified Mac.Native          as Native

-- | A screen rectangle, in points.
data Rect = Rect
  { rectX :: {-# UNPACK #-} !Int
  , rectY :: {-# UNPACK #-} !Int
  , rectW :: {-# UNPACK #-} !Int
  , rectH :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | A native window identifier paired with its absolute screen rectangle.
data Window = Window
  { windowId   :: {-# UNPACK #-} !Int
  -- | Absolute screen rectangle used to position pointer gestures.
  , windowRect :: !Rect
  }
  deriving (Eq, Show)

-- | Centre of a rectangle, in points.
windowCenter :: Rect -> (Int, Int)
windowCenter (Rect x y w h) = (x + w `div` 2, y + h `div` 2)

-- | Select the largest portrait phone window and ignore tiny guard dialogs.
selectPhoneWindow :: [Rect] -> Maybe Rect
selectPhoneWindow rects =
  case filter phoneLike rects of
    []         -> Nothing
    candidates -> Just (maximumBy (comparing area) candidates)
  where
    phoneLike Rect {rectW, rectH} =
      rectW >= 200
        && rectH >= 400
        && rectH > rectW
    area Rect {rectW, rectH} = rectW * rectH

-- | Locate a window by application name (e.g. @"iPhone Mirroring"@). 'Nothing'
-- when the app has no on-screen phone-shaped layer-zero window.
findWindow :: String -> IO (Maybe Window)
findWindow appName =
  selectNativeWindow <$> Native.listWindows appName
  where
    selectNativeWindow windows = do
      rect <- selectPhoneWindow (map toRect windows)
      (nativeId, _, _, _, _) <-
        find ((== rect) . toRect) windows
      pure (Window nativeId rect)
    toRect (_, x, y, width, height) = Rect x y width height

-- | Bring an application to the foreground. Capture works without this, but
-- gestures only register when the app is frontmost.
focusApp :: String -> IO ()
focusApp = Native.focusApp

-- | Grab a screen region directly as packed RGB pixels. ScreenCaptureKit uses
-- native display pixels, so Retina captures contain two pixels per point.
captureFrame :: Window -> IO (Image PixelRGB8)
captureFrame = Native.captureRgb . windowId
