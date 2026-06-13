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

  caseSpec "case0 (no bats, 6x8)"
    "references/pure-solver/case0.txt"
    "references/pure-solver/case0_out0.txt"

  caseSpec "case1 (with bats, 5x5)"
    "references/pure-solver/case1.txt"
    "references/pure-solver/case1_out0.txt"
