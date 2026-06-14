-- | Replay solver moves as macOS swipe gestures.
--
-- Each swipe is an absolute pointer drag from the window centre about 100px
-- in the move's direction, split into a few evenly spaced pointer events.
module Mac.Gesture
  ( swipe
  , swipeTarget
  , swipePath
  , imagePointToScreen
  , clickPoint
  , replay
  ) where

import           Board
import           Mac.Mirror
import qualified Mac.Native          as Native
import           UnliftIO.Concurrent

-- | Distance, in points, dragged from the window centre for a swipe.
swipeDelta :: Int
swipeDelta = 100

-- | Time after a swipe for the gravity animation to settle, in microseconds.
moveSettleDelay :: Int
moveSettleDelay = 1000000

-- | Number of intermediate drag events in one swipe.
swipeSteps :: Int
swipeSteps = 5

-- | Endpoint of the drag for a gravity direction (window centre offset by
-- 'swipeDelta').
swipeTarget :: Rect -> Dir -> (Int, Int)
swipeTarget rect dir = case dir of
  U -> (cx, cy - swipeDelta)
  D -> (cx, cy + swipeDelta)
  L -> (cx - swipeDelta, cy)
  R -> (cx + swipeDelta, cy)
  where
    (cx, cy) = windowCenter rect

-- | Absolute screen points for one swipe, including its start and endpoint.
swipePath :: Rect -> Dir -> [(Int, Int)]
swipePath rect dir = (cx, cy) : dragPoints
  where
    (cx, cy) = windowCenter rect
    (tx, ty) = swipeTarget rect dir
    dragPoints =
      [ ( cx + ((tx - cx) * step) `div` swipeSteps
        , cy + ((ty - cy) * step) `div` swipeSteps
        )
      | step <- [1 .. swipeSteps]
      ]

-- | Convert a point in captured-image pixels to an absolute screen point.
imagePointToScreen :: Rect -> (Int, Int) -> (Int, Int) -> (Int, Int)
imagePointToScreen rect (imageWidth, imageHeight) (imageX, imageY) =
  ( rectX rect + scale imageX (rectW rect) imageWidth
  , rectY rect + scale imageY (rectH rect) imageHeight
  )
  where
    fi = fromIntegral
    scale value screenExtent imageExtent =
      round (fi value * fi screenExtent / fi imageExtent :: Double)

-- | Click one absolute screen point.
clickPoint :: (Int, Int) -> IO ()
clickPoint = Native.click

-- | One weak gravity swipe. Returns 'False' after yielding to pointer input.
swipe :: Rect -> Dir -> IO Bool
swipe rect dir = fmap (== Native.DragCompleted) (Native.drag (swipePath rect dir))

-- | Replay a solution until complete or interrupted by pointer input.
replay :: Rect -> [Dir] -> IO Bool
replay rect = go
  where
    go [] = pure True
    go (d : ds) = do
      completed <- swipe rect d
      if completed
        then threadDelay moveSettleDelay >> go ds
        else pure False
