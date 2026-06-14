-- | The bot's command-line entry point.
--
-- Subcommands keep each layer independently runnable, so the pure halves can be
-- exercised with zero hardware and the macOS halves checked in isolation:
--
--   * @solve \<board.txt\>@  — text board in, move sequence out (no CV, no mac)
--   * @parse \<frame.png\>@  — CV only: classify a frame, print the glyph board
--   * @capture [out.png]@    — mac only: grab the game window to a PNG
--   * @swipe \<dir\>@        — mac only: issue one gravity swipe
--   * @run@ (default)        — the full capture -> parse -> solve -> replay loop
--
-- The mirrored app name defaults to \"iPhone Mirroring\" and can be overridden
-- with the @GSB_APP@ environment variable.
module Main (main) where

import           Board
import           Control.Concurrent
import           Data.Char
import           Image
import           Mac.Gesture
import           Mac.Mirror
import           Solve
import           System.Environment
import           System.Exit
import           Vision.Board
import           Vision.Screen

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["solve", path]  -> runSolve path
    ["parse", path]  -> runParse path
    ["capture"]      -> runCapture "frame.png"
    ["capture", out] -> runCapture out
    ["swipe", dir]   -> runSwipe dir
    ["run"]          -> runFull
    []               -> runFull
    _                -> usage

usage :: IO ()
usage = die $
  unlines
    [ "gems-seeker-bot — Gem Seeker solver"
    , ""
    , "  solve <board.txt>            solve a text board, print the moves"
    , "  parse <frame.png>            classify a frame, print the glyph board"
    , "  capture [out.png]            grab the game window to a PNG (default frame.png)"
    , "  swipe <up|down|left|right>   issue one gravity swipe"
    , "  run                          capture, parse, solve, and replay (default)"
    , ""
    , "App name override: GSB_APP (default \"iPhone Mirroring\")."
    ]

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
    Left err    -> die ("parse failed: " ++ err)
    Right board -> putStr (renderBoard board ++ "\n")

runCapture :: FilePath -> IO ()
runCapture out = do
  app <- appName
  window <- requireWindow
  focusApp app -- bring the game forward so we grab it, not whatever is on top
  threadDelay 500000
  frame <- captureFrame window
  writePng out frame
  putStrLn $
    "captured "
      ++ show (imageWidth frame)
      ++ "x"
      ++ show (imageHeight frame)
      ++ " RGB pixels to "
      ++ out

runSwipe :: String -> IO ()
runSwipe dirText =
  case parseDir dirText of
    Nothing -> die ("unknown direction: " ++ dirText ++ " (use up|down|left|right)")
    Just dir -> do
      app <- appName
      window <- requireWindow
      focusApp app
      completed <- swipe (windowRect window) dir
      if completed
        then putStrLn ("swiped " ++ show dir)
        else putStrLn "stopped: pointer input interrupted swipe"

runFull :: IO ()
runFull = do
  app <- appName
  templates <- loadTemplates
  playTemplate <- loadRGB8 "assets/templates/play.png"
  focusApp app
  loop app templates playTemplate 0 0

loop :: String -> Templates -> Image PixelRGB8 -> Int -> Int -> IO ()
loop app templates playTemplate consecutivePlayClicks transientRetries = do
  window <- requireWindow
  let rect = windowRect window
  frame <- captureFrame window
  case findPlayButton playTemplate frame of
    Just imagePoint
      | consecutivePlayClicks < 3 -> do
          let screenPoint =
                imagePointToScreen
                  rect
                  (imageWidth frame, imageHeight frame)
                  imagePoint
          putStrLn ("PLAY detected; clicking " ++ show screenPoint)
          focusApp app
          clickPoint screenPoint
          threadDelay 500000
          loop app templates playTemplate (consecutivePlayClicks + 1) 0
      | otherwise ->
          putStrLn "stopped: PLAY remained visible after 3 click attempts"
    Nothing ->
      case parseBoard templates [frame] of
        Left err
          | transientRetries < 10 -> do
              threadDelay 500000
              loop app templates playTemplate consecutivePlayClicks (transientRetries + 1)
          | otherwise ->
              putStrLn ("stopped: no PLAY button and parse failed: " ++ err)
        Right board -> do
          putStrLn "parsed board:"
          putStrLn (renderBoard board)
          case solve board of
            Nothing -> putStrLn "stopped: no solution found"
            Just moves -> do
              putStrLn (show (length moves) ++ " moves: " ++ unwords (map show moves))
              focusApp app
              completed <- replay rect moves
              if completed
                then do
                  putStrLn "done"
                  threadDelay 500000
                  loop app templates playTemplate 0 0
                else
                  putStrLn "stopped: pointer input interrupted replay"

-- helpers --------------------------------------------------------------------

reportSolution :: Board -> IO ()
reportSolution board =
  case solve board of
    Nothing -> die "no solution found"
    Just moves ->
      putStrLn (show (length moves) ++ " moves: " ++ unwords (map show moves))

-- | Read a board file: the case-file format (@count@, @"W H"@, grid) when it
-- looks like one, otherwise a bare glyph grid.
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

requireWindow :: IO Window
requireWindow = do
  app <- appName
  findWindow app >>= \case
    Just rect -> pure rect
    Nothing ->
      die $
        unlines
          [ "could not read the '" ++ app ++ "' window."
          , "  - Is the app open with its phone window visible on screen?"
          ]

appName :: IO String
appName = maybe "iPhone Mirroring" id <$> lookupEnv "GSB_APP"

parseDir :: String -> Maybe Dir
parseDir s = case map toLower s of
  "up"    -> Just U
  "u"     -> Just U
  "down"  -> Just D
  "d"     -> Just D
  "left"  -> Just L
  "l"     -> Just L
  "right" -> Just R
  "r"     -> Just R
  _       -> Nothing
