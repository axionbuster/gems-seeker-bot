-- | Top-level solver: turn a board into the optimal sequence of gravity moves.
--
-- Ported from @references/pure-solver@ (@src/SolveTotM2.hs@). The search runs
-- over @'Either' ('Exc' 'Board') 'Board'@: an ongoing game is @Right b@, a
-- finished one is @Left (Won b)@ or @Left Lost@. Only @Right@ states expand, and
-- the goal is @Left (Won _)@ — so a winning move (one that leaves no gems) is the
-- terminal we search for, and losing moves become dead ends with no successors.
module Solve
  ( solve
  , parseCase
  ) where

import           Board           (Board (..), Dir (..), Exc (..), allDirs,
                                  applyGravity, boardFromLines)
import           Search.Dijkstra (dijkstra)

-- | The optimal move sequence that collects every gem, if one exists. The empty
-- list is returned for a board that already has no gems.
solve :: Board -> Maybe [Dir]
solve board0 = snd <$> dijkstra start isGoal successors
  where
    start :: Either (Exc Board) Board
    start = Right board0

    successors (Right b) = [(d, applyGravity d b) | d <- allDirs]
    successors (Left _)  = []

    isGoal (Left (Won _)) = True
    isGoal _              = False

-- | Parse the reference case-file format: a leading case count, a @"W H"@ line,
-- then @H@ grid rows. Only the first case is read.
parseCase :: String -> Board
parseCase s = case lines s of
  (_count : dims : rest) ->
    let h = case words dims of
          [_, hh] -> read hh
          _       -> error "parseCase: expected \"<width> <height>\" on line 2"
     in boardFromLines (take h rest)
  _ -> error "parseCase: expected at least a count line and a dimensions line"
