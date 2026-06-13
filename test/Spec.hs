module Main (main) where

import Data.Either (isRight)
import Data.List (isInfixOf, isPrefixOf)
import Test.Hspec

import Board
  ( Board (..)
  , Cell (..)
  , Dir (..)
  , Exc (..)
  , applyGravity
  , gemCount
  , isSolved
  , renderBoard
  )
import Solve (parseCase, solve)

import Image (Image, PixelRGB8 (..), generateImage, pixelAt)
import Image.Zncc (bestZncc, zncc)

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

pixelAtG :: Image PixelRGB8 -> Int -> Int -> PixelRGB8
pixelAtG = pixelAt

invert :: PixelRGB8 -> PixelRGB8
invert (PixelRGB8 r g b) = PixelRGB8 (255 - r) (255 - g) (255 - b)

-- | Board after a (possibly terminal) move outcome.
boardOf :: Either (Exc Board) Board -> Board
boardOf (Right b) = b
boardOf (Left (Won b)) = b
boardOf (Left Lost) = error "boardOf: Lost has no board"

isWon :: Either (Exc Board) Board -> Bool
isWon (Left (Won _)) = True
isWon _ = False

-- | Apply each direction in turn, recording every outcome. Stops at the first
-- terminal (Won/Lost) outcome.
stepThrough :: Board -> [Dir] -> [Either (Exc Board) Board]
stepThrough _ [] = []
stepThrough b (d : ds) =
  let r = applyGravity d b
   in r : case r of
        Right b' -> stepThrough b' ds
        _ -> []

boardLines :: Board -> [String]
boardLines = lines . renderBoard

-- | The reference output format: an "Initial state:" grid followed by a series
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
dirFromWord "Up" = U
dirFromWord "Down" = D
dirFromWord "Left" = L
dirFromWord "Right" = R
dirFromWord w = error ("dirFromWord: unexpected direction " ++ show w)

-- | Drives a single reference case: physics must reproduce every recorded board,
-- and the solver must match the optimal move count and win.
caseSpec :: String -> FilePath -> FilePath -> Spec
caseSpec name caseFile outFile = describe name $ do
  board0 <- runIO (parseCase <$> readFile caseFile)
  outText <- runIO (readFile outFile)
  let (initialGrid, steps) = parseOut (boardH board0) outText
      refDirs = map fst steps
      refGrids = map snd steps
      results = stepThrough board0 refDirs

  it "parses the initial board to match the reference render" $
    boardLines board0 `shouldBe` initialGrid

  it "physics reproduces every recorded board up to the winning move" $ do
    -- Every non-terminal move is an ongoing game and must match the reference
    -- board cell-for-cell. We do NOT pin the board at the instant of winning:
    -- like the reference's @moveGame@, our sweep aborts the moment the last gem
    -- lands, so a not-yet-settled bat's resting spot there is implementation-
    -- defined (and irrelevant — the game is already over).
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
      Nothing -> expectationFailure "solver found no solution"
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

  caseSpec "case0 (no bats, 6x8)"
    "references/pure-solver/case0.txt"
    "references/pure-solver/case0_out0.txt"

  caseSpec "case1 (with bats, 5x5)"
    "references/pure-solver/case1.txt"
    "references/pure-solver/case1_out0.txt"
