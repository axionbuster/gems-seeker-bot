module Main (main) where

import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Image (decodePng)
import Mac.Gesture (replay)
import Mac.Mirror (captureFrame, findWindow)
import Solve (solve)
import Vision.Board (parseBoard)

-- | The intended end-to-end flow, not yet wired (scaffold). We capture the
-- board programmatically (a frame or two is enough), parse it once, solve it
-- completely, then replay the swipes.
_pipeline :: IO ()
_pipeline = do
  mregion <- findWindow "iPhone Mirroring"
  case mregion of
    Nothing -> hPutStrLn stderr "capture window not found"
    Just region -> do
      png <- captureFrame region
      case decodePng png of
        Left err -> hPutStrLn stderr ("decode failed: " <> err)
        Right frame -> case parseBoard [frame] of
          Left err -> hPutStrLn stderr ("parse failed: " <> err)
          Right board -> case solve board of
            Nothing -> hPutStrLn stderr "no solution"
            Just moves -> replay region moves

main :: IO ()
main = do
  hPutStrLn stderr "gems-seeker-bot: scaffold — see _pipeline for the intended flow."
  exitFailure
