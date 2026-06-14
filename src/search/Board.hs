-- | Board state and gravity physics.
--
-- Ported from @references/pure-solver@ (@src/TotM2.hs@). The reference uses a
-- bit-packed @Word@ array (2 bits/cell) for speed; here we keep a plain
-- row-major @[Cell]@ in the public type (correctness-first, matches the search
-- state) and thaw into a boxed @STArray@ only inside 'applyGravity'. The slide
-- algorithm itself is a faithful port: a single recursive @go@ (the reference's
-- @go0@ and its blocker-push @chain ... there@ are the same closure), with
-- 'Won'\/'Lost' thrown mid-sweep via 'ExceptT'.
--
-- The player slot (glyph @*@) is modelled as a 'Player' cell sitting at the
-- target location — the reference tracks it as a separate coordinate whose cell
-- is air. A movable piece whose next cell is the player is consumed there: a gem
-- is collected (removed), a bat loses. The player cell is never moved or
-- overwritten, so it behaves exactly like the reference's air sink.
--
-- Carrying the packed representation over is a documented optimisation for later
-- (see references/pure-solver docs/INTERNING_PLAN.md); per-state hashing of the
-- cell list is the known cost. Correctness and tests come first.
module Board
  ( Cell (..)
  , Dir (..)
  , Board (..)
  , Exc (..)
  , allDirs
  , applyGravity
  , isSolved
  , gemCount
  , boardFromLines
  , renderBoard
  , cellFromChar
  , cellToChar
  ) where

import           Control.Monad              (unless, when)
import           Control.Monad.ST           (ST, runST)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import           Data.Array                 (Array, elems, listArray)
import           Data.Array.MArray          (freeze, readArray, thaw,
                                             writeArray)
import           Data.Array.ST              (STArray)
import           Data.Hashable              (Hashable)
import           Data.STRef                 (newSTRef, readSTRef, writeSTRef)
import           GHC.Generics               (Generic)

-- | One grid cell. Glyphs: @.@ air, @\@@ gem, @%@ bat, @#@ wall, @*@ player.
data Cell = Air | Gem | Bat | Wall | Player
  deriving (Eq, Ord, Show, Generic)

instance Hashable Cell

-- | Gravity direction for a move.
data Dir = U | D | L | R
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

instance Hashable Dir

-- | The four moves, in a stable order.
allDirs :: [Dir]
allDirs = [minBound .. maxBound]

-- | A board: row-major cells of a @boardW@ x @boardH@ grid.
data Board = Board
  { boardW     :: {-# UNPACK #-} !Int
  , boardH     :: {-# UNPACK #-} !Int
  , boardCells :: ![Cell]
  }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Board

-- | The outcome of a move when the game ends: a win carries the final board; a
-- loss carries nothing. Mirrors @TotM2.Exc@.
data Exc a = Won a | Lost
  deriving (Eq, Ord, Show, Generic)

instance Hashable a => Hashable (Exc a)

-- | Number of gems still on the board.
gemCount :: Board -> Int
gemCount = length . filter (== Gem) . boardCells

-- | Only gems and bats slide under gravity.
movable :: Cell -> Bool
movable Gem = True
movable Bat = True
movable _   = False

-- | Apply gravity: every movable piece slides until it hits a wall, a boundary,
-- or another piece; a gem sliding onto the player is collected, a bat sliding
-- onto the player loses.
--
-- @Left (Won b)@: the move collected the last gem (game won, @b@ is the board at
-- that instant). @Left Lost@: a bat reached the player. @Right b@: the game
-- continues with board @b@.
applyGravity :: Dir -> Board -> Either (Exc Board) Board
applyGravity dir b0
  | gemCount b0 == 0 = Left (Won b0)
  | otherwise = runST (run b0)
  where
    h = boardH b0
    w = boardW b0
    next = case dir of
      U -> \(r, c) -> (r - 1, c)
      D -> \(r, c) -> (r + 1, c)
      L -> \(r, c) -> (r, c - 1)
      R -> \(r, c) -> (r, c + 1)
    inb (r, c) = r >= 0 && r < h && c >= 0 && c < w

    run :: forall s. Board -> ST s (Either (Exc Board) Board)
    run b = do
      arr <- thaw (listArray ((0, 0), (h - 1, w - 1)) (boardCells b))
              :: ST s (STArray s (Int, Int) Cell)
      gems <- newSTRef (gemCount b)
      let freezeBoard = do
            a <- freeze arr :: ST s (Array (Int, Int) Cell)
            pure (Board w h (elems a))
          -- Slide the piece at @pos@ as far as it goes. Mirrors TotM2's @go0@;
          -- the reference's blocker push @chain ... there@ is this same call on
          -- @there@, so one recursive function covers both.
          go :: (Int, Int) -> ExceptT (Exc ()) (ST s) ()
          go pos = when (inb pos) $ do
            this <- lift (readArray arr pos)
            let there = next pos
            when (movable this && inb there) $ do
              that <- lift (readArray arr there)
              case that of
                Player -> case this of
                  Bat -> throwE Lost
                  _ -> do
                    n <- lift (readSTRef gems)
                    if n > 1
                      then lift $ do
                        writeSTRef gems (n - 1)
                        writeArray arr pos Air
                      else do
                        lift $ do
                          writeSTRef gems 0
                          writeArray arr pos Air
                        throwE (Won ())
                Air -> do
                  lift $ do
                    writeArray arr there this
                    writeArray arr pos Air
                  go there
                Wall -> pure ()
                _ -> do
                  -- movable blocker ahead: push it, then retry if it cleared.
                  go there
                  that' <- lift (readArray arr there)
                  unless (movable that') (go pos)
      res <- runExceptT $
        sequence_ [go (r, c) | r <- [0 .. h - 1], c <- [0 .. w - 1]]
      case res of
        Left Lost -> pure (Left Lost)
        Left (Won ()) -> Left . Won <$> freezeBoard
        Right () -> do
          n <- readSTRef gems
          if n == 0 then Left . Won <$> freezeBoard else Right <$> freezeBoard

-- | A board is solved once no gems remain.
isSolved :: Board -> Bool
isSolved = notElem Gem . boardCells

-- | Glyph -> cell. Anything unrecognised is treated as air.
cellFromChar :: Char -> Cell
cellFromChar '@' = Gem
cellFromChar '%' = Bat
cellFromChar '#' = Wall
cellFromChar '*' = Player
cellFromChar _   = Air

-- | Cell -> glyph.
cellToChar :: Cell -> Char
cellToChar Air    = '.'
cellToChar Gem    = '@'
cellToChar Bat    = '%'
cellToChar Wall   = '#'
cellToChar Player = '*'

-- | Build a board from grid lines (height = number of lines, width = length of
-- the first line). Trailing short lines are not padded; callers pass clean grids.
boardFromLines :: [String] -> Board
boardFromLines [] = Board 0 0 []
boardFromLines ls@(l0 : _) =
  Board (length l0) (length ls) (concatMap (map cellFromChar) ls)

-- | Render a board back to glyph lines, newline-separated (no trailing newline).
renderBoard :: Board -> String
renderBoard (Board w _ cells) =
  unlinesNoTrailing (map (map cellToChar) (chunks w cells))
  where
    chunks _ [] = []
    chunks n xs = let (a, b) = splitAt n xs in a : chunks n b
    unlinesNoTrailing = foldr1NL
    foldr1NL []       = ""
    foldr1NL [x]      = x
    foldr1NL (x : xs) = x ++ "\n" ++ foldr1NL xs
