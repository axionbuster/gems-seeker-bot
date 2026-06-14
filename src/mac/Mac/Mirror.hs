-- | macOS control of the mirroring app: locate its window, grab a region of the
-- screen, and (via "Mac.Gesture") swipe it. Everything is in screen *points*,
-- so there is no retina/DPI conversion to do.
--
-- IO goes through @unliftio@.
module Mac.Mirror
  ( Rect (..)
  , windowCenter
  , parseGeometry
  , parseGeometries
  , selectPhoneWindow
  , geometryScript
  , findWindow
  , focusApp
  , captureFrame
  ) where

import           Control.Monad    (void)
import           Data.ByteString  (ByteString)
import           Data.List        (maximumBy)
import           Data.Ord         (comparing)
import qualified Mac.Native       as Native
import           System.Exit      (ExitCode (..))
import           UnliftIO.Process (readProcessWithExitCode)

-- | A screen rectangle, in points.
data Rect = Rect
  { rectX :: {-# UNPACK #-} !Int
  , rectY :: {-# UNPACK #-} !Int
  , rectW :: {-# UNPACK #-} !Int
  , rectH :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Show)

-- | Centre of a rectangle, in points.
windowCenter :: Rect -> (Int, Int)
windowCenter (Rect x y w h) = (x + w `div` 2, y + h `div` 2)

-- | The AppleScript that prints one @"x,y,w,h"@ line for every window of
-- @appName@. iPhone Mirroring can expose a tiny guard dialog before its actual
-- phone window, so callers must inspect the complete list.
geometryScript :: String -> String
geometryScript appName =
  unlines
    [ "tell application \"System Events\" to tell process \"" ++ appName ++ "\""
    , "  set output to \"\""
    , "  repeat with w in windows"
    , "    try"
    , "      set p to position of w"
    , "      set s to size of w"
    , "      set output to output & (item 1 of p as string) & \",\""
        ++ " & (item 2 of p as string) & \",\""
        ++ " & (item 1 of s as string) & \",\""
        ++ " & (item 2 of s as string) & linefeed"
    , "    end try"
    , "  end repeat"
    , "  return output"
    , "end tell"
    ]

-- | Parse @"x,y,w,h"@ (integers, or tolerant of @"100.0"@ forms) into a 'Rect'.
parseGeometry :: String -> Maybe Rect
parseGeometry raw =
  case map readInt (splitOn ',' (trim raw)) of
    [Just x, Just y, Just w, Just h] -> Just (Rect x y w h)
    _                                -> Nothing
  where
    readInt s = case reads (trim s) :: [(Double, String)] of
      [(d, "")] -> Just (round d)
      _         -> Nothing

-- | Parse every non-empty geometry line emitted by 'geometryScript'.
parseGeometries :: String -> [Rect]
parseGeometries = mapMaybe parseGeometry . filter (not . null) . lines

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
-- when the app is not running, inaccessible, or has no phone-shaped window.
findWindow :: String -> IO (Maybe Rect)
findWindow appName = do
  (code, out, _err) <-
    readProcessWithExitCode "osascript" ["-e", geometryScript appName] ""
  pure $ case code of
    ExitSuccess   -> selectPhoneWindow (parseGeometries out)
    ExitFailure _ -> Nothing

-- | Bring an application to the foreground. Capture works without this, but
-- gestures only register when the app is frontmost.
focusApp :: String -> IO ()
focusApp appName =
  void $
    readProcessWithExitCode
      "osascript"
      ["-e", "tell application \"" ++ appName ++ "\" to activate"]
      ""

-- | Grab a screen region as PNG bytes. ScreenCaptureKit returns native display
-- pixels, so Retina captures contain two pixels per screen point.
captureFrame :: Rect -> IO ByteString
captureFrame (Rect x y w h) = Native.capturePng x y w h

-- helpers ---------------------------------------------------------------------

splitOn :: Char -> String -> [String]
splitOn sep = foldr step [""]
  where
    step c acc@(cur : rest)
      | c == sep = "" : acc
      | otherwise = (c : cur) : rest
    step _ [] = [""]

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = foldr step []
  where
    step value rest =
      case f value of
        Just result -> result : rest
        Nothing     -> rest

trim :: String -> String
trim = f . f
  where
    f = reverse . dropWhile (`elem` " \t\r\n")
