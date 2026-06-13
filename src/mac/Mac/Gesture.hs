-- | Replay solver moves as macOS swipe gestures.
--
-- TODO: port the drag geometry from references/experiments
-- (@src/mac/Mac/Mirror.hs@, @scrollAll@), driving @cliclick@.
module Mac.Gesture
  ( swipe
  , replay
  ) where

import Board (Dir (..))
import Mac.Mirror (Rect)

-- | One gravity swipe within a region.
swipe :: Rect -> Dir -> IO ()
swipe _region dir = case dir of
  U -> pure ()
  D -> pure ()
  L -> pure ()
  R -> pure ()

-- | Replay a full solution as a sequence of swipes.
replay :: Rect -> [Dir] -> IO ()
replay region = mapM_ (swipe region)
