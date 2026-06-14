-- | Board state and gravity physics.
--
-- The public board stays row-major and easy to inspect; the hot path only
-- thaws into an ST array while a gravity sweep runs. Gems collect on the player
-- cell, bats lose, and the player cell itself never moves or gets overwritten.
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

import           Control.Monad
import           Control.Monad.ST
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Data.Array
import           Data.Array.MArray
import           Data.Array.ST
import           Data.Hashable
import           Data.STRef
import           GHC.Generics

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
          -- Slide the piece at @pos@ as far as it goes. One recursive function
          -- handles both the active piece and any blocker it pushes ahead.
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
