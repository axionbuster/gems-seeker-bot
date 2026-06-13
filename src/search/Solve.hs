-- | Top-level solver: turn a board into the optimal sequence of gravity moves.
--
-- TODO: track the direction taken on each edge so the move list can be
-- recovered from the search path; port move scoring from
-- references/pure-solver (@src/SolveTotM2.hs@).
module Solve
  ( solve
  ) where

import Board (Board, Dir, allDirs, applyGravity, isSolved)
import Search.Dijkstra (dijkstra)

-- | The optimal move sequence that collects every gem, if one exists.
solve :: Board -> Maybe [Dir]
solve start = movesAlong <$> dijkstra start isSolved successors
  where
    successors b = [(applyGravity d b, 1) | d <- allDirs]
    movesAlong _path = []  -- TODO: recover the directions from the state path
