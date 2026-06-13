-- | Board state and gravity physics.
--
-- This is the place the bit-packed representation and slide logic from
-- @references/pure-solver@ (@src/TotM2.hs@) gets ported. The list-of-cells
-- representation below is a correctness-first placeholder; replace it with the
-- packed layout once tests pin the behaviour down. Performance matters here.
module Board
  ( Cell (..)
  , Dir (..)
  , Board (..)
  , allDirs
  , applyGravity
  , isSolved
  ) where

-- | One grid cell. Glyphs: @.@ air, @\@@ gem, @%@ bat, @#@ wall, @*@ player.
data Cell = Air | Gem | Bat | Wall | Player
  deriving (Eq, Ord, Show)

-- | Gravity direction for a move.
data Dir = U | D | L | R
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | The four moves, in a stable order.
allDirs :: [Dir]
allDirs = [minBound .. maxBound]

-- | A board: row-major cells of a @boardW@ x @boardH@ grid.
data Board = Board
  { boardW :: {-# UNPACK #-} !Int
  , boardH :: {-# UNPACK #-} !Int
  , boardCells :: ![Cell]
  }
  deriving (Eq, Ord, Show)

-- | Apply gravity: every movable piece slides until it hits a wall, a boundary,
-- or another piece; gems sliding onto the player are collected.
--
-- TODO: port the slide/collection physics from references/pure-solver (TotM2).
applyGravity :: Dir -> Board -> Board
applyGravity _dir b = b

-- | A board is solved once no gems remain.
isSolved :: Board -> Bool
isSolved = notElem Gem . boardCells
