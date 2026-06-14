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

import           Control.Concurrent (threadDelay)
import           Control.Monad      (filterM, unless)
import qualified Data.ByteString    as BS
import           Data.Char          (isDigit, toLower)
import           Data.Maybe         (isJust)
import           System.Directory   (doesFileExist, findExecutable)
import           System.Environment (getArgs, lookupEnv)
import           System.Exit        (exitFailure)
import           System.IO          (hPutStrLn, stderr)

import           Board              (Board, Dir (..), boardFromLines,
                                     renderBoard)
import           Image              (Image, PixelRGB8, convertRGB8, decodeImage,
                                     imageHeight, imageWidth, readImage)
import           Mac.Gesture        (clickPoint, imagePointToScreen, replay,
                                     swipe)
import           Mac.Mirror         (Rect, captureFrame, findWindow, focusApp)
import           Solve              (parseCase, solve)
import           Vision.Board       (Templates, parseBoard, prepareTemplates)
import           Vision.Screen      (findPlayButton)

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
    Left err    -> die ("parse failed: " ++ err)
    Right board -> putStr (renderBoard board ++ "\n")

runCapture :: FilePath -> IO ()
runCapture out = do
  probeSystemDependencies
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
      probeSystemDependencies
      app <- appName
      rect <- requireWindow
      focusApp app
      swipe rect dir
      putStrLn ("swiped " ++ show dir)

runFull :: IO ()
runFull = do
  probeSystemDependencies
  app <- appName
  templates <- loadTemplates
  playTemplate <- loadRGB8 "assets/templates/play.png"
  focusApp app
  loop app templates playTemplate 0 0

loop :: String -> Templates -> Image PixelRGB8 -> Int -> Int -> IO ()
loop app templates playTemplate consecutivePlayClicks transientRetries = do
  rect <- requireWindow
  png <- captureFrame rect
  BS.writeFile "frame.png" png
  case decodeImage png of
    Left err -> die ("decode failed: " ++ err)
    Right dynamic -> do
      let frame = convertRGB8 dynamic
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
                  replay rect moves
                  putStrLn "done"
                  threadDelay 500000
                  loop app templates playTemplate 0 0

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

data SystemDependency = SystemDependency
  { dependencyName :: String
  , dependencyHint :: String
  , dependencyCheck :: IO Bool
  }

probeSystemDependencies :: IO ()
probeSystemDependencies = do
  missing <- filterM (fmap not . dependencyCheck) systemDependencies
  unless (null missing) $
    die $
      unlines
        ("missing system dependencies:" : map formatDependency missing)
  where
    formatDependency dep =
      "  - " ++ dependencyName dep ++ " (" ++ dependencyHint dep ++ ")"

systemDependencies :: [SystemDependency]
systemDependencies =
  [ SystemDependency
      { dependencyName = "screencapture"
      , dependencyHint = "ships with macOS"
      , dependencyCheck = binaryAvailable "screencapture" ["/usr/sbin/screencapture"]
      }
  , SystemDependency
      { dependencyName = "osascript"
      , dependencyHint = "ships with macOS"
      , dependencyCheck = binaryAvailable "osascript" ["/usr/bin/osascript"]
      }
  , SystemDependency
      { dependencyName = "cliclick"
      , dependencyHint = "install with `brew install cliclick`"
      , dependencyCheck = binaryAvailable "cliclick" ["/opt/homebrew/bin/cliclick", "/usr/local/bin/cliclick"]
      }
  ]

binaryAvailable :: String -> [FilePath] -> IO Bool
binaryAvailable name fallbackPaths = do
  inPath <- isJust <$> findExecutable name
  inFallback <- or <$> mapM doesFileExist fallbackPaths
  pure (inPath || inFallback)

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

die :: String -> IO a
die msg = hPutStrLn stderr msg >> exitFailure
