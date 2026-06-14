-- | Replay solver moves as macOS swipe gestures.
--
-- Ported from @references/experiments@ (@src/mac/Mac/Mirror.hs@, @scroll@), an
-- approach already proven to drive the live game. Each swipe is an absolute
-- @cliclick@ drag from the window centre ~100px in the move's direction, so no
-- human cursor placement is needed; the drag shape and timing match the
-- known-good reference values.
module Mac.Gesture
  ( swipe
  , swipeTarget
  , cliclickArgs
  , replay
  ) where

import Board (Dir (..))
import Mac.Mirror (Rect, windowCenter)
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Process (callProcess)

-- | Distance, in points, dragged from the window centre for a swipe.
swipeDelta :: Int
swipeDelta = 100

-- | Brief pause before pressing, in milliseconds. This leaves the app enough
-- time to receive focus without slowing every move to the old two-second pace.
preSwipeDelayMs :: Int
preSwipeDelayMs = 100

-- | Time after a swipe for the gravity animation to settle, in microseconds.
moveSettleDelay :: Int
moveSettleDelay = 250000

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

-- | The @cliclick@ argument vector for one swipe: settle, press at centre, drag
-- to the target, release. Pure so it can be unit-tested.
cliclickArgs :: Rect -> Dir -> [String]
cliclickArgs rect dir =
  [ "-e", "500"
  , "w:" ++ show preSwipeDelayMs
  , "m:" ++ point (cx, cy)
  , "dd:" ++ point (cx, cy)
  , "dm:" ++ point (tx, ty)
  , "du:" ++ point (tx, ty)
  ]
  where
    (cx, cy) = windowCenter rect
    (tx, ty) = swipeTarget rect dir
    point (x, y) = show x ++ "," ++ show y

-- | One gravity swipe within a region.
swipe :: Rect -> Dir -> IO ()
swipe rect dir = callProcess "cliclick" (cliclickArgs rect dir)

-- | Replay a full solution as a sequence of swipes, pausing briefly so
-- the board settles. Focus the target app first (see 'Mac.Mirror.focusApp').
replay :: Rect -> [Dir] -> IO ()
replay rect = go
  where
    go [] = pure ()
    go (d : ds) = do
      swipe rect d
      threadDelay moveSettleDelay
      go ds
