-- | Private FFI bindings for the Objective-C macOS integration.
module Mac.Native
  ( DragResult (..)
  , captureRgb
  , listWindows
  , focusApp
  , click
  , drag
  ) where

import           Codec.Picture.Types
import           Control.Exception
import qualified Data.Vector.Storable  as VS
import           Data.Word
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable

-- | Whether a weak drag completed or yielded to other pointer input.
data DragResult = DragCompleted | DragInterrupted
  deriving (Eq, Show)

-- | Capture one native window as a packed RGB image.
captureRgb :: Int -> IO (Image PixelRGB8)
captureRgb windowId =
  alloca $ \pixelsOut ->
    alloca $ \widthOut ->
      alloca $ \heightOut ->
        alloca $ \errorOut -> do
          poke pixelsOut nullPtr
          poke widthOut 0
          poke heightOut 0
          poke errorOut nullPtr
          result <-
            cCaptureRgb
              (fromIntegral windowId)
              pixelsOut
              widthOut
              heightOut
              errorOut
          if result == 0
            then do
              pixels <- peek pixelsOut
              capturedWidth <- fromIntegral <$> peek widthOut
              capturedHeight <- fromIntegral <$> peek heightOut
              if pixels == nullPtr || capturedWidth <= 0 || capturedHeight <= 0
                then do
                  if pixels == nullPtr then pure () else cFree pixels
                  ioError (userError "native screen capture returned invalid RGB data")
                else do
                  foreignPixels <- newForeignPtr cFreeFinalizer pixels
                  let pixelCount = capturedWidth * capturedHeight * 3
                  pure
                    Image
                      { imageWidth  = capturedWidth
                      , imageHeight = capturedHeight
                      , imageData = VS.unsafeFromForeignPtr0 foreignPixels pixelCount
                      }
            else throwNativeError "native screen capture failed" errorOut

-- | List layer-zero on-screen windows owned by an application.
listWindows :: String -> IO [(Int, Int, Int, Int, Int)]
listWindows appName =
  withCString appName $ \appNamePointer ->
    alloca $ \coordinatesOut ->
      alloca $ \countOut ->
        alloca $ \errorOut -> do
          poke coordinatesOut nullPtr
          poke countOut 0
          poke errorOut nullPtr
          result <-
            cListWindows
              appNamePointer
              coordinatesOut
              countOut
              errorOut
          if result == 0
            then do
              coordinates <- peek coordinatesOut
              count <- fromIntegral <$> peek countOut
              if coordinates == nullPtr
                then pure []
                else do
                  values <-
                    peekArray (count * 5) coordinates
                      `finally` cFree coordinates
                  pure (toRects (map fromIntegral values))
            else throwNativeError "native window lookup failed" errorOut
  where
    toRects (windowId : x : y : width : height : rest) =
      (windowId, x, y, width, height) : toRects rest
    toRects _ = []

-- | Bring a running application and all of its windows to the foreground.
focusApp :: String -> IO ()
focusApp appName =
  withCString appName $ \appNamePointer ->
    withNativeError "native application activation failed" $ \errorOut ->
      cFocusApp appNamePointer errorOut

-- | Post one primary-button click at an absolute screen point.
click :: (Int, Int) -> IO ()
click (x, y) =
  withNativeError "native click failed" $ \errorOut ->
    cClick (fromIntegral x) (fromIntegral y) errorOut

-- | Drag through absolute screen points, yielding to other pointer input.
drag :: [(Int, Int)] -> IO DragResult
drag points
  | length points < 2 = ioError (userError "native drag requires at least two points")
  | otherwise =
      withArray coordinates $ \coordinatesPtr ->
        alloca $ \errorOut -> do
          poke errorOut nullPtr
          result <-
            cDrag
              coordinatesPtr
              (fromIntegral (length points))
              errorOut
          case result of
            0 -> pure DragCompleted
            2 -> pure DragInterrupted
            _ -> throwNativeError "native drag failed" errorOut
  where
    coordinates =
      concatMap (\(x, y) -> [fromIntegral x, fromIntegral y]) points

withNativeError :: String -> (Ptr CString -> IO CInt) -> IO ()
withNativeError fallback action =
  alloca $ \errorOut -> do
    poke errorOut nullPtr
    result <- action errorOut
    if result == 0
      then pure ()
      else throwNativeError fallback errorOut

throwNativeError :: String -> Ptr CString -> IO a
throwNativeError fallback errorOut = do
  errorPointer <- peek errorOut
  if errorPointer == nullPtr
    then ioError (userError fallback)
    else do
      message <- peekCString errorPointer `finally` cFree errorPointer
      ioError (userError message)

foreign import ccall safe "gsb_capture_rgb"
  cCaptureRgb
    :: CUInt
    -> Ptr (Ptr Word8)
    -> Ptr CInt
    -> Ptr CInt
    -> Ptr CString
    -> IO CInt

foreign import ccall safe "gsb_list_windows"
  cListWindows
    :: CString
    -> Ptr (Ptr CInt)
    -> Ptr CSize
    -> Ptr CString
    -> IO CInt

foreign import ccall safe "gsb_focus_app"
  cFocusApp :: CString -> Ptr CString -> IO CInt

foreign import ccall safe "gsb_click"
  cClick :: CInt -> CInt -> Ptr CString -> IO CInt

foreign import ccall safe "gsb_drag"
  cDrag :: Ptr CInt -> CSize -> Ptr CString -> IO CInt

foreign import ccall unsafe "gsb_free"
  cFree :: Ptr a -> IO ()

foreign import ccall unsafe "&gsb_free"
  cFreeFinalizer :: FinalizerPtr a
