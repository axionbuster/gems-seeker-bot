{-# LANGUAGE CPP #-}

module Main (main) where

import           Control.Monad    (filterM)
import           Data.Either      (isRight)
import           Data.List        (isInfixOf, isPrefixOf)
import           System.Directory (doesFileExist)
import           Test.Hspec

import           Board            (Board (..), Cell (..), Dir (..), Exc (..),
                                   applyGravity, gemCount, isSolved,
                                   renderBoard)
import           Solve            (parseCase, solve)

import           Image            (Image, PixelRGB8 (..), convertRGB8,
                                   generateImage, pixelAt, readImage)
import           Image.Frame      (resizeNearest)
import           Image.Zncc       (bestZncc, zncc)
import           Vision.Board     (parseBoard, prepareTemplates,
                                   validateParsedBoard)
import           Vision.Screen    (findPlayButton)

#ifdef DARWIN
import           Mac.Gesture      (imagePointToScreen, swipePath, swipeTarget)
import           Mac.Mirror       (Rect (..), parseGeometries, parseGeometry,
                                   selectPhoneWindow, windowCenter)
#endif

-- A non-flat RGB gradient image (non-zero variance, so ZNCC is well-defined).
gradient :: Int -> Int -> Image PixelRGB8
gradient w h = generateImage px w h
  where
    px x y =
      PixelRGB8
        (fromIntegral ((x * 37 + y * 17) `mod` 256))
        (fromIntegral ((x * 11 + y * 53) `mod` 256))
        (fromIntegral ((x * 101 + y * 7) `mod` 256))

near :: Double -> Double -> Bool
near a b = abs (a - b) < 1e-9

loadRGB8 :: FilePath -> IO (Image PixelRGB8)
loadRGB8 path =
  either (error . ((path ++ ": ") ++)) convertRGB8 <$> readImage path

pixelAtG :: Image PixelRGB8 -> Int -> Int -> PixelRGB8
pixelAtG = pixelAt

invert :: PixelRGB8 -> PixelRGB8
invert (PixelRGB8 r g b) = PixelRGB8 (255 - r) (255 - g) (255 - b)

-- | Board after a (possibly terminal) move outcome.
boardOf :: Either (Exc Board) Board -> Board
boardOf (Right b)      = b
boardOf (Left (Won b)) = b
boardOf (Left Lost)    = error "boardOf: Lost has no board"

isWon :: Either (Exc Board) Board -> Bool
isWon (Left (Won _)) = True
isWon _              = False

-- | Apply each direction in turn, recording every outcome. Stops at the first
-- terminal (Won/Lost) outcome.
stepThrough :: Board -> [Dir] -> [Either (Exc Board) Board]
stepThrough _ [] = []
stepThrough b (d : ds) =
  let r = applyGravity d b
   in r : case r of
        Right b' -> stepThrough b' ds
        _        -> []

boardLines :: Board -> [String]
boardLines = lines . renderBoard

-- | The sample output format: an "Initial state:" grid followed by a series
-- of "Step k: Apply gravity DIR" blocks, each with its resulting grid. Returns
-- the initial grid and the list of (direction, resulting-grid) steps.
parseOut :: Int -> String -> ([String], [(Dir, [String])])
parseOut h s = (initialGrid, go afterInitial)
  where
    ls = lines s
    afterMarker marker = drop 1 (dropWhile (not . (marker `isInfixOf`)) ls)
    initialGrid = take h (afterMarker "Initial state:")
    -- everything after the initial grid block
    afterInitial = drop h (afterMarker "Initial state:")

    go [] = []
    go (line : rest)
      | "Step " `isPrefixOf` line && "Apply gravity" `isInfixOf` line =
          let d = dirFromWord (last (words line))
              grid = take h rest
           in (d, grid) : go (drop h rest)
      | otherwise = go rest

dirFromWord :: String -> Dir
dirFromWord "Up"    = U
dirFromWord "Down"  = D
dirFromWord "Left"  = L
dirFromWord "Right" = R
dirFromWord w       = error ("dirFromWord: unexpected direction " ++ show w)

-- | Drives a single sample case: physics must reproduce every recorded board,
-- and the solver must match the optimal move count and win.
caseSpec :: String -> FilePath -> FilePath -> Spec
caseSpec name caseFile outFile = describe name $ do
  board0 <- runIO (parseCase <$> readFile caseFile)
  outText <- runIO (readFile outFile)
  let (initialGrid, steps) = parseOut (boardH board0) outText
      refDirs = map fst steps
      refGrids = map snd steps
      results = stepThrough board0 refDirs

  it "parses the initial board to match the sample render" $
    boardLines board0 `shouldBe` initialGrid

  it "physics reproduces every recorded board up to the winning move" $ do
    -- Every non-terminal move is an ongoing game and must match the board
    -- cell-for-cell. We do not pin the board at the instant of winning: the
    -- sweep aborts the moment the last gem lands, so a not-yet-settled bat's
    -- resting spot there is implementation-defined.
    length results `shouldBe` length steps
    let nonFinal = init results
    all isRight nonFinal `shouldBe` True
    map (boardLines . boardOf) nonFinal `shouldBe` init refGrids

  it "physics wins with no gems and the player intact on the final move" $ do
    let final = last results
    isWon final `shouldBe` True
    gemCount (boardOf final) `shouldBe` 0
    (Player `elem` boardCells (boardOf final)) `shouldBe` True

  it "solver finds a solution of the optimal length" $
    fmap length (solve board0) `shouldBe` Just (length steps)

  it "solver's own move sequence wins when replayed" $
    case solve board0 of
      Nothing   -> expectationFailure "solver found no solution"
      Just mine -> isWon (last (stepThrough board0 mine)) `shouldBe` True

main :: IO ()
main = hspec $ do
  describe "Board.isSolved" $ do
    it "holds when no gems remain" $
      isSolved (Board 1 1 [Air]) `shouldBe` True
    it "fails while a gem remains" $
      isSolved (Board 2 1 [Gem, Air]) `shouldBe` False

  describe "Board.applyGravity" $ do
    it "slides a gem to the far wall" $
      applyGravity R (Board 3 1 [Gem, Air, Air])
        `shouldBe` Right (Board 3 1 [Air, Air, Gem])

    it "is a no-op when the gem already rests against the wall" $
      applyGravity L (Board 3 1 [Gem, Air, Air])
        `shouldBe` Right (Board 3 1 [Gem, Air, Air])

    it "collects the last gem onto the player and wins" $
      applyGravity R (Board 3 1 [Gem, Air, Player])
        `shouldBe` Left (Won (Board 3 1 [Air, Air, Player]))

    it "loses when a bat reaches the player" $
      -- a gem is present so the move is actually simulated (not short-circuited)
      applyGravity R (Board 3 2 [Gem, Air, Air, Bat, Air, Player])
        `shouldBe` Left Lost

  describe "Image.Zncc.zncc" $ do
    it "scores a perfect 1 for an identical window" $
      zncc (gradient 6 6) (gradient 6 6) (0, 0) `shouldSatisfy` near 1

    it "scores -1 for a perfectly inverted window" $ do
      let t = gradient 6 6
          s = generateImage (\x y -> invert (pixelAtG t x y)) 6 6
      zncc t s (0, 0) `shouldSatisfy` near (-1)

    it "scores 0 for a flat (zero-variance) template" $
      zncc (generateImage (\_ _ -> PixelRGB8 7 7 7) 6 6) (gradient 6 6) (0, 0)
        `shouldBe` 0

    it "scores 0 when the template does not fit at the offset" $ do
      let t = gradient 6 6
          s = gradient 6 6
      zncc t s (1, 0) `shouldBe` 0 -- 6-wide template can't sit at x=1 in a 6-wide source
    it "scores 0 for a negative offset" $
      zncc (gradient 4 4) (gradient 8 8) (-1, 0) `shouldBe` 0

    it "finds the embedded template via bestZncc within the search radius" $ do
      let t = gradient 4 4
          -- source: t embedded at (2,3), constant gray elsewhere
          s = generateImage embed 9 9
          embed x y =
            let lx = x - 2
                ly = y - 3
             in if lx >= 0 && lx < 4 && ly >= 0 && ly < 4
                  then pixelAtG t lx ly
                  else PixelRGB8 128 128 128
      bestZncc t s 3 (2, 3) `shouldSatisfy` near 1

#ifdef DARWIN
  describe "Mac.Mirror.parseGeometry" $ do
    it "parses integer geometry" $
      parseGeometry "100,200,300,400\n" `shouldBe` Just (Rect 100 200 300 400)
    it "tolerates decimal geometry" $
      parseGeometry " 100.0 , 200.0 , 300.0 , 400.0 " `shouldBe` Just (Rect 100 200 300 400)
    it "rejects malformed geometry" $
      parseGeometry "not,a,rect" `shouldBe` Nothing

    it "parses every valid window geometry" $
      parseGeometries "23,39,66,20\n17,30,348,766\nbad\n"
        `shouldBe` [Rect 23 39 66 20, Rect 17 30 348 766]

    it "selects the phone window after a tiny guard dialog" $
      selectPhoneWindow [Rect 23 39 66 20, Rect 17 30 348 766]
        `shouldBe` Just (Rect 17 30 348 766)

    it "rejects a window list containing only guard dialogs" $
      selectPhoneWindow [Rect 23 39 66 20]
        `shouldBe` Nothing

  describe "Mac gesture geometry" $ do
    let rect = Rect 0 0 200 200 -- centre (100,100)
    it "centres the window" $
      windowCenter rect `shouldBe` (100, 100)
    it "aims each swipe 100px from centre" $ do
      swipeTarget rect U `shouldBe` (100, 0)
      swipeTarget rect D `shouldBe` (100, 200)
      swipeTarget rect L `shouldBe` (0, 100)
      swipeTarget rect R `shouldBe` (200, 100)
    it "builds a short linear drag path" $
      swipePath rect R
        `shouldBe`
          [ (100, 100)
          , (120, 100)
          , (140, 100)
          , (160, 100)
          , (180, 100)
          , (200, 100)
          ]

    it "maps Retina image pixels into screen points" $
      imagePointToScreen (Rect 17 30 348 766) (696, 1532) (348, 1362)
        `shouldBe` (191, 711)
#endif

  -- CV oracle: the bundled frame set should classify to the same board from a
  -- few of them. Skips cleanly if the local frames or fixture are not present.
  describe "Vision.Board.parseBoard (golden: scene1 frames)" $ do
    let gemPath = "assets/templates/gem.png"
        batPath = "assets/templates/bat.png"
        framePaths =
          [ "test/fixtures/frames/scene1-1.png"
          , "test/fixtures/frames/scene1-2.png"
          , "test/fixtures/frames/scene1-3.png"
          ]
        fixturePath = "test/fixtures/frames/scene1.board"
        required = gemPath : batPath : fixturePath : framePaths
    missing <- runIO (filterM (fmap not . doesFileExist) required)
    if not (null missing)
      then it "reproduces the consensus board" $
        pendingWith ("missing fixtures: " ++ show missing)
      else do
        result <- runIO $ do
          gemT <- loadRGB8 gemPath
          batT <- loadRGB8 batPath
          frames <- mapM loadRGB8 framePaths
          pure (parseBoard (prepareTemplates gemT batT) frames)
        expected <- runIO (readFile fixturePath)
        it "reproduces the consensus board" $
          fmap (lines . renderBoard) result `shouldBe` Right (lines expected)

  describe "Vision.Board.parseBoard (live iPhone Mirroring capture)" $ do
    let framePath = "test/fixtures/frames/live-window.png"
        fixturePath = "test/fixtures/frames/live-window.board"
        required =
          [ "assets/templates/gem.png"
          , "assets/templates/bat.png"
          , framePath
          , fixturePath
          ]
    missing <- runIO (filterM (fmap not . doesFileExist) required)
    if not (null missing)
      then it "parses the current theme through surrounding window chrome" $
        pendingWith ("missing fixtures: " ++ show missing)
      else do
        result <- runIO $ do
          gemT <- loadRGB8 "assets/templates/gem.png"
          batT <- loadRGB8 "assets/templates/bat.png"
          frame <- loadRGB8 framePath
          pure (parseBoard (prepareTemplates gemT batT) [frame])
        expected <- runIO (readFile fixturePath)
        it "parses the current theme through surrounding window chrome" $
          fmap (lines . renderBoard) result `shouldBe` Right (lines expected)

  describe "Vision.Board.parseBoard (live brown theme)" $ do
    let framePath = "test/fixtures/frames/live-brown-window.png"
        fixturePath = "test/fixtures/frames/live-brown-window.board"
        required =
          [ "assets/templates/gem.png"
          , "assets/templates/bat.png"
          , framePath
          , fixturePath
          ]
    missing <- runIO (filterM (fmap not . doesFileExist) required)
    if not (null missing)
      then it "separates the yellow player from isolated interior walls" $
        pendingWith ("missing fixtures: " ++ show missing)
      else do
        result <- runIO $ do
          gemT <- loadRGB8 "assets/templates/gem.png"
          batT <- loadRGB8 "assets/templates/bat.png"
          frame <- loadRGB8 framePath
          pure (parseBoard (prepareTemplates gemT batT) [frame])
        expected <- runIO (readFile fixturePath)
        it "separates the yellow player from isolated interior walls" $
          fmap (lines . renderBoard) result `shouldBe` Right (lines expected)

  describe "Vision.Board.parseBoard (half-cell grid phase)" $ do
    let framePath = "test/fixtures/frames/live-orange-window.png"
        fixturePath = "test/fixtures/frames/live-orange-window.board"
        required =
          [ "assets/templates/gem.png"
          , "assets/templates/bat.png"
          , framePath
          , fixturePath
          ]
    missing <- runIO (filterM (fmap not . doesFileExist) required)
    if not (null missing)
      then it "recovers a board whose sprites use the alternate lattice phase" $
        pendingWith ("missing fixtures: " ++ show missing)
      else do
        result <- runIO $ do
          gemT <- loadRGB8 "assets/templates/gem.png"
          batT <- loadRGB8 "assets/templates/bat.png"
          frame <- loadRGB8 framePath
          pure (parseBoard (prepareTemplates gemT batT) [frame])
        expected <- runIO (readFile fixturePath)
        it "recovers a board whose sprites use the alternate lattice phase" $
          fmap (lines . renderBoard) result `shouldBe` Right (lines expected)

  describe "Vision.Board.parseBoard (segmented perimeter theme)" $ do
    let framePath = "test/fixtures/frames/live-purple-window.png"
        fixturePath = "test/fixtures/frames/live-purple-window.board"
        required =
          [ "assets/templates/gem.png"
          , "assets/templates/bat.png"
          , framePath
          , fixturePath
          ]
    missing <- runIO (filterM (fmap not . doesFileExist) required)
    if not (null missing)
      then it "combines separated wall runs into one playfield envelope" $
        pendingWith ("missing fixtures: " ++ show missing)
      else do
        result <- runIO $ do
          gemT <- loadRGB8 "assets/templates/gem.png"
          batT <- loadRGB8 "assets/templates/bat.png"
          frame <- loadRGB8 framePath
          pure (parseBoard (prepareTemplates gemT batT) [frame])
        expected <- runIO (readFile fixturePath)
        it "combines separated wall runs into one playfield envelope" $
          fmap (lines . renderBoard) result `shouldBe` Right (lines expected)

        scaledResult <- runIO $ do
          gemT <- loadRGB8 "assets/templates/gem.png"
          batT <- loadRGB8 "assets/templates/bat.png"
          frame <- loadRGB8 framePath
          pure (parseBoard (prepareTemplates gemT batT) [resizeNearest 348 766 frame])
        it "scales sprite templates to a one-point-per-pixel capture" $
          fmap (lines . renderBoard) scaledResult `shouldBe` Right (lines expected)

  describe "Vision.Board.validateParsedBoard" $ do
    it "accepts one player with at least one gem" $
      validateParsedBoard (Board 2 1 [Player, Gem])
        `shouldBe` Right (Board 2 1 [Player, Gem])

    it "rejects a scene with multiple player cells" $
      validateParsedBoard (Board 3 1 [Player, Player, Gem])
        `shouldBe` Left "Vision.Board.parseBoard: expected exactly one player, found 2"

    it "rejects a scene without a remaining gem" $
      validateParsedBoard (Board 1 1 [Player])
        `shouldBe` Left "Vision.Board.parseBoard: expected at least one gem"

  describe "Vision.Screen.findPlayButton" $ do
    let templatePath = "assets/templates/play.png"
        playFramePath = "test/fixtures/frames/live-play-window.png"
        boardFramePath = "test/fixtures/frames/live-window.png"
        required = [templatePath, playFramePath, boardFramePath]
    missing <- runIO (filterM (fmap not . doesFileExist) required)
    if not (null missing)
      then it "finds PLAY without matching a game board" $
        pendingWith ("missing fixtures: " ++ show missing)
      else do
        playResult <- runIO $ do
          template <- loadRGB8 templatePath
          playFrame <- loadRGB8 playFramePath
          boardFrame <- loadRGB8 boardFramePath
          pure
            ( findPlayButton template playFrame
            , findPlayButton template boardFrame
            , findPlayButton template (resizeNearest 348 766 playFrame)
            )
        it "finds PLAY without matching a game board" $
          playResult `shouldBe` (Just (348, 1362), Nothing, Just (173, 681))

  caseSpec "case0 (no bats, 6x8)"
    "test/fixtures/cases/case0.txt"
    "test/fixtures/cases/case0.out"

  caseSpec "case1 (with bats, 5x5)"
    "test/fixtures/cases/case1.txt"
    "test/fixtures/cases/case1.out"
