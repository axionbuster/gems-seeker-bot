-- | The bot's command-line entry point.
--
-- Subcommands keep each layer independently runnable, so the pure halves can be
-- exercised with zero hardware and the macOS halves checked in isolation:
--
--   * @solve \<board.txt\>@  — text board in, move sequence out (no CV, no mac)
--   * @parse \<frame.png\>@  — CV only: classify a frame, print the glyph board
--   * @capture [out.png]@    — mac only: grab the game window to a PNG
--   * @swipe \<dir\>@        — mac only: issue one gravity swipe
--   * @almost@               — solve once and replay all but the winning move
--   * @run@ (default)        — the full capture -> parse -> solve -> replay loop
--
-- Live gameplay modes record timestamped movies unless @--no-record@ is set.
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
  if any (`elem` ["--help", "-h"]) args
    then putStr usageText
    else dispatch args

dispatch :: [String] -> IO ()
dispatch args =
  let suppressRecording = "--no-record" `elem` args
      command = filter (/= "--no-record") args
   in case command of
        ["solve", path]
          | not suppressRecording -> runSolve path
        ["parse", path]
          | not suppressRecording -> runParse path
        ["capture"]
          | not suppressRecording -> runCapture "frame.png"
        ["capture", out]
          | not suppressRecording -> runCapture out
        ["swipe", dir] -> runSwipe (not suppressRecording) dir
        ["almost"]     -> runAlmost (not suppressRecording)
        ["run"]        -> runFull (not suppressRecording)
        []             -> runFull (not suppressRecording)
        _              -> usageError

usageText :: String
usageText =
  unlines
    [ "gems-seeker-bot — Gem Seeker solver"
    , ""
    , "Usage:"
    , "  gems-seeker-bot [--help]"
    , "  gems-seeker-bot [--no-record] [run]"
    , "  gems-seeker-bot [--no-record] almost"
    , "  gems-seeker-bot [--no-record] swipe <up|down|left|right>"
    , "  gems-seeker-bot solve <board.txt>"
    , "  gems-seeker-bot parse <frame.png>"
    , "  gems-seeker-bot capture [out.png]"
    , ""
    , "Commands:"
    , "  solve <board.txt>            solve a text board, print the moves"
    , "  parse <frame.png>            classify a frame, print the glyph board"
    , "  capture [out.png]            grab the game window to a PNG (default frame.png)"
    , "  swipe <up|down|left|right>   issue one gravity swipe"
    , "  almost                       solve once, replay all but the final move"
    , "  run                          capture, parse, solve, and replay (default)"
    , ""
    , "Options:"
    , "  --help, -h                   show this help"
    , "  --no-record                  suppress recording for run, almost, or swipe"
    , ""
    , "Movies: recordings/<timestamp>-<mode>.mov (app audio, up to 120 FPS, 2 pixels/point)."
    , "App name override: GSB_APP (default \"iPhone Mirroring\")."
    ]

usageError :: IO ()
usageError = die usageText

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

runSwipe :: Bool -> String -> IO ()
runSwipe shouldRecord dirText =
  case parseDir dirText of
    Nothing -> die ("unknown direction: " ++ dirText ++ " (use up|down|left|right)")
    Just dir -> do
      app <- appName
      window <- requireWindow
      focusApp app
      completed <-
        withGameplayRecording shouldRecord "swipe" window $
          swipe (windowRect window) dir
      if completed
        then putStrLn ("swiped " ++ show dir)
        else putStrLn "stopped: pointer input interrupted swipe"

runAlmost :: Bool -> IO ()
runAlmost shouldRecord = do
  app <- appName
  templates <- loadTemplates
  playTemplate <- loadRGB8 "assets/templates/play.png"
  focusApp app
  window <- requireWindow
  withGameplayRecording shouldRecord "almost" window $
    almost app templates playTemplate 0 0

-- | Delay between one-shot screen-state checks, in microseconds.
almostPollDelay :: Int
almostPollDelay = 300000

almost :: String -> Templates -> Image PixelRGB8 -> Int -> Int -> IO ()
almost app templates playTemplate consecutivePlayClicks transientRetries = do
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
          threadDelay almostPollDelay
          almost app templates playTemplate (consecutivePlayClicks + 1) 0
      | otherwise ->
          putStrLn "stopped: PLAY remained visible after 3 click attempts"
    Nothing ->
      case parseBoard templates [frame] of
        Left err
          | transientRetries < 10 -> do
              threadDelay almostPollDelay
              almost app templates playTemplate consecutivePlayClicks (transientRetries + 1)
          | otherwise ->
              putStrLn ("stopped: no PLAY button and parse failed: " ++ err)
        Right board -> do
          putStrLn "parsed board:"
          putStrLn (renderBoard board)
          case solve board of
            Nothing -> putStrLn "stopped: no solution found"
            Just moves ->
              case almostMoves moves of
                Nothing ->
                  putStrLn "stopped: solution has no move to withhold"
                Just movesToReplay -> do
                  putStrLn $
                    "almost: replaying "
                      ++ show (length movesToReplay)
                      ++ " of "
                      ++ show (length moves)
                      ++ " moves: "
                      ++ unwords (map show movesToReplay)
                  focusApp app
                  completed <- replay rect movesToReplay
                  if completed
                    then putStrLn "done: final move left unreplayed"
                    else putStrLn "stopped: pointer input interrupted replay"

runFull :: Bool -> IO ()
runFull shouldRecord = do
  app <- appName
  templates <- loadTemplates
  playTemplate <- loadRGB8 "assets/templates/play.png"
  focusApp app
  window <- requireWindow
  withGameplayRecording shouldRecord "run" window $
    loop app templates playTemplate 0 0

-- | Main loop: capture the game window, detect the PLAY button, parse the board, solve, and replay moves.
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

withGameplayRecording :: Bool -> String -> Window -> IO a -> IO a
withGameplayRecording False _ _ action = action
withGameplayRecording True mode window action = do
  outputPath <- newRecordingPath mode
  result <-
    withRecording window outputPath $ \info -> do
      putStrLn $
        "recording "
          ++ show (recordingWidth info)
          ++ "x"
          ++ show (recordingHeight info)
          ++ " at up to "
          ++ show (recordingFps info)
          ++ " FPS to "
          ++ recordingPath info
      action
  putStrLn ("saved recording to " ++ outputPath)
  pure result

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
