-- | macOS control of the mirroring app: locate and focus its native window,
-- capture RGB pixels, and (via "Mac.Gesture") swipe it. Window geometry is in
-- screen points; captured image dimensions use native display pixels.
module Mac.Mirror
  ( Rect (..)
  , Window
  , windowRect
  , RecordingInfo (..)
  , windowCenter
  , selectPhoneWindow
  , findWindow
  , focusApp
  , captureFrame
  , recordingFilePath
  , newRecordingPath
  , withRecording
  ) where

import           Codec.Picture.Types
import           Data.List
import           Data.Ord
import           Data.Time
import qualified Mac.Native          as Native
import           System.Directory
import           System.FilePath
import           UnliftIO.Exception

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
  , windowRect ::                !Rect -- ^ Absolute screen rectangle for pointer gestures.
  }
  deriving (Eq, Show)

-- | Properties of the movie stream selected for a recording.
data RecordingInfo = RecordingInfo
  { recordingPath   :: FilePath
  , recordingWidth  :: {-# UNPACK #-} !Int
  , recordingHeight :: {-# UNPACK #-} !Int
  , recordingFps    :: {-# UNPACK #-} !Int
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

-- | Build a timestamped movie path for a live mode.
recordingFilePath :: FilePath -> String -> ZonedTime -> FilePath
recordingFilePath directory mode timestamp =
  directory
    </> formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S%Q%z" timestamp
    ++ "-"
    ++ mode
    <.> "mov"

-- | Create the ignored recording directory and choose a timestamped movie path.
newRecordingPath :: String -> IO FilePath
newRecordingPath mode = do
  let directory = "recordings"
  createDirectoryIfMissing True directory
  recordingFilePath directory mode <$> getZonedTime

-- | Record a window around an action, finalizing the movie on every exit path.
withRecording :: Window -> FilePath -> (RecordingInfo -> IO a) -> IO a
withRecording window outputPath =
  bracket start (const Native.stopRecording)
  where
    start = do
      (width, height, fps) <-
        Native.startRecording (windowId window) outputPath 120 2
      pure
        RecordingInfo
          { recordingPath = outputPath
          , recordingWidth = width
          , recordingHeight = height
          , recordingFps = fps
          }
