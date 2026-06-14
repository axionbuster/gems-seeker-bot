-- | Private FFI bindings for the Objective-C macOS integration.
module Mac.Native
  ( DragResult (..)
  , capturePng
  , click
  , drag
  ) where

import           Control.Exception     (finally)
import qualified Data.ByteString       as BS
import           Data.Word             (Word8)
import           Foreign.C.String      (CString, peekCString)
import           Foreign.C.Types       (CInt (..), CSize (..))
import           Foreign.Marshal.Alloc (alloca)
import           Foreign.Marshal.Array (withArray)
import           Foreign.Ptr           (Ptr, castPtr, nullPtr)
import           Foreign.Storable      (peek, poke)

-- | Whether a weak drag completed or yielded to other pointer input.
data DragResult = DragCompleted | DragInterrupted
  deriving (Eq, Show)

-- | Capture a global screen rectangle and return PNG bytes.
capturePng :: Int -> Int -> Int -> Int -> IO BS.ByteString
capturePng x y width height =
  alloca $ \bytesOut ->
    alloca $ \lengthOut ->
      alloca $ \errorOut -> do
        poke bytesOut nullPtr
        poke lengthOut 0
        poke errorOut nullPtr
        result <-
          cCapturePng
            (fromIntegral x)
            (fromIntegral y)
            (fromIntegral width)
            (fromIntegral height)
            bytesOut
            lengthOut
            errorOut
        if result == 0
          then do
            bytes <- peek bytesOut
            byteCount <- peek lengthOut
            if bytes == nullPtr
              then ioError (userError "native screen capture returned no PNG data")
              else
                BS.packCStringLen (castPtr bytes, fromIntegral byteCount)
                  `finally` cFree bytes
          else throwNativeError "native screen capture failed" errorOut

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

foreign import ccall safe "gsb_capture_png"
  cCapturePng
    :: CInt
    -> CInt
    -> CInt
    -> CInt
    -> Ptr (Ptr Word8)
    -> Ptr CSize
    -> Ptr CString
    -> IO CInt

foreign import ccall safe "gsb_click"
  cClick :: CInt -> CInt -> Ptr CString -> IO CInt

foreign import ccall safe "gsb_drag"
  cDrag :: Ptr CInt -> CSize -> Ptr CString -> IO CInt

foreign import ccall unsafe "gsb_free"
  cFree :: Ptr a -> IO ()
