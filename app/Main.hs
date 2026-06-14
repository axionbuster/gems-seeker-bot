-- | The bot's command-line entry point.
--
-- Subcommands keep each layer independently runnable, so the pure halves can be
-- exercised with zero hardware and the macOS halves checked in isolation:
--
--   * @solve \<board.txt\>@  — text board in, move sequence out (no CV, no mac)
--   * @parse \<frame.png\>@  — CV only: classify a frame, print the glyph board
--   * @capture [out.png]@   — mac only: grab the game window to a PNG
--   * @swipe \<dir\>@        — mac only: issue one gravity swipe
--   * @run@ (default)       — the full capture -> parse -> solve -> replay loop
--
-- The mirrored app name defaults to \"iPhone Mirroring\" and can be overridden
-- with the @GSB_APP@ environment variable.
module Main (main) where

import Control.Concurrent (threadDelay)
import Data.ByteString qualified as BS
import Data.Char (isDigit, toLower)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Board (Board, Dir (..), boardFromLines, renderBoard)
import Image (Image, PixelRGB8, convertRGB8, decodeImage, readImage)
import Mac.Gesture (replay, swipe)
import Mac.Mirror (Rect, captureFrame, findWindow, focusApp)
import Solve (parseCase, solve)
import Vision.Board (Templates, parseBoard, prepareTemplates)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["solve", path] -> runSolve path
    ["parse", path] -> runParse path
    ["capture"] -> runCapture "frame.png"
    ["capture", out] -> runCapture out
    ["swipe", dir] -> runSwipe dir
    ["run"] -> runFull
    [] -> runFull
    _ -> usage

usage :: IO ()
usage = do
  hPutStrLn stderr $
    unlines
      [ "gems-seeker-bot — Gem Seeker solver"
      , ""
      , "  solve <board.txt>   solve a text board, print the moves"
      , "  parse <frame.png>   classify a frame, print the glyph board"
      , "  capture [out.png]   grab the game window to a PNG (default frame.png)"
      , "  swipe <up|down|left|right>   issue one gravity swipe"
      , "  run                 capture, parse, solve, and replay (default)"
      , ""
      , "App name override: GSB_APP (default \"iPhone Mirroring\")."
      ]
  exitFailure

-- subcommands ----------------------------------------------------------------

runSolve :: FilePath -> IO ()
runSolve path = do
  board <- readBoardFile path
  reportSolution board

runParse :: FilePath -> IO ()
runParse path = do
  templates <- loadTemplates
  frame <- loadRGB8 path
  case parseBoard templates [frame] of
    Left err -> die ("parse failed: " ++ err)
    Right board -> putStr (renderBoard board ++ "\n")

runCapture :: FilePath -> IO ()
runCapture out = do
  app <- appName
  rect <- requireWindow
  focusApp app -- bring the game forward so we grab it, not whatever is on top
  threadDelay 500000
  png <- captureFrame rect
  BS.writeFile out png
  putStrLn ("captured " ++ show (BS.length png) ++ " bytes to " ++ out)

runSwipe :: String -> IO ()
runSwipe dirText =
  case parseDir dirText of
    Nothing -> die ("unknown direction: " ++ dirText ++ " (use up|down|left|right)")
    Just dir -> do
      app <- appName
      rect <- requireWindow
      focusApp app
      swipe rect dir
      putStrLn ("swiped " ++ show dir)

runFull :: IO ()
runFull = do
  app <- appName
  templates <- loadTemplates
  rect <- requireWindow
  focusApp app
  png <- captureFrame rect
  BS.writeFile "frame.png" png -- keep the frame around for debugging
  case decodeImage png of
    Left err -> die ("decode failed: " ++ err)
    Right dynamic ->
      case parseBoard templates [convertRGB8 dynamic] of
        Left err -> die ("parse failed: " ++ err)
        Right board -> do
          putStrLn "parsed board:"
          putStrLn (renderBoard board)
          case solve board of
            Nothing -> die "no solution found"
            Just moves -> do
              putStrLn (show (length moves) ++ " moves: " ++ unwords (map show moves))
              replay rect moves
              putStrLn "done"

-- helpers --------------------------------------------------------------------

reportSolution :: Board -> IO ()
reportSolution board =
  case solve board of
    Nothing -> die "no solution found"
    Just moves ->
      putStrLn (show (length moves) ++ " moves: " ++ unwords (map show moves))

-- | Read a board file: the reference case format (@count@, @"W H"@, grid) when
-- it looks like one, otherwise a bare glyph grid.
readBoardFile :: FilePath -> IO Board
readBoardFile path = do
  contents <- readFile path
  pure $ case lines contents of
    (countLine : dimsLine : _)
      | all isDigit (trim countLine)
      , [_, _] <- words dimsLine ->
          parseCase contents
    ls -> boardFromLines (filter (not . null) ls)
  where
    trim = dropWhile (== ' ') . reverse . dropWhile (== ' ') . reverse

loadTemplates :: IO Templates
loadTemplates = do
  gemT <- loadRGB8 "assets/templates/gem.png"
  batT <- loadRGB8 "assets/templates/bat.png"
  pure (prepareTemplates gemT batT)

loadRGB8 :: FilePath -> IO (Image PixelRGB8)
loadRGB8 path =
  readImage path >>= \case
    Left err -> die (path ++ ": " ++ err)
    Right dynamic -> pure (convertRGB8 dynamic)

requireWindow :: IO Rect
requireWindow = do
  app <- appName
  findWindow app >>= \case
    Just rect -> pure rect
    Nothing ->
      die $
        unlines
          [ "could not read the '" ++ app ++ "' window."
          , "  - Is the app open and showing the game?"
          , "  - Grant this terminal Accessibility permission in"
          , "    System Settings > Privacy & Security > Accessibility."
          ]

appName :: IO String
appName = maybe "iPhone Mirroring" id <$> lookupEnv "GSB_APP"

parseDir :: String -> Maybe Dir
parseDir s = case map toLower s of
  "up" -> Just U
  "u" -> Just U
  "down" -> Just D
  "d" -> Just D
  "left" -> Just L
  "l" -> Just L
  "right" -> Just R
  "r" -> Just R
  _ -> Nothing

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure
