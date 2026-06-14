-- | Generic uniform-cost (Dijkstra) search with move tracking.
--
-- The frontier is threaded as an explicit @(queue, costs)@ pair, which keeps
-- the implementation small without an extra dependency. Each settled state
-- stores the edge label that reached it, so the solution is reconstructed
-- directly rather than re-derived by diffing states.
--
-- @HashPSQ@ keys are unique, so reinserting a state with a lower cost is a
-- proper decrease-key; with non-negative edge costs a popped state is final.
module Search.Dijkstra
  ( dijkstra
  ) where

import           Data.Hashable       (Hashable)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import qualified Data.HashPSQ        as Q

-- | How a state was reached: either the start (no predecessor) or a step
-- carrying its cost, predecessor, and the edge label taken. @Step@ is listed
-- first because it is by far the more common constructor.
data Node s h = Step {-# UNPACK #-} !Int s h | Start

-- | Lowest-cost path from @start@ to any goal state, with the moves taken.
--
-- @successors@ yields each neighbour with the move label that reaches it; every
-- edge costs 1 (uniform cost). Returns @(states, moves)@ where @states@ runs
-- from @start@ through to the goal and @moves@ are the edge labels between them
-- (@length moves == length states - 1@). 'Nothing' if no goal is reachable.
dijkstra
  :: forall s h. (Hashable s, Ord s)
  => s                   -- ^ start state
  -> (s -> Bool)         -- ^ goal predicate
  -> (s -> [(h, s)])     -- ^ labelled successors (each edge costs 1)
  -> Maybe ([s], [h])
dijkstra start isGoal successors =
  go (Q.singleton start (0 :: Int) ()) (M.singleton start Start)
  where
    go bq costs = case Q.minView bq of
      Nothing -> Nothing
      Just (ux, ud, _, bq1)
        | isGoal ux -> Just (recon costs ux)
        | otherwise ->
            let (bq2, costs2) = foldl' (relax ux ud) (bq1, costs) (successors ux)
             in go bq2 costs2

    relax ux ud (bq, costs) (how, vx) =
      let vd0 = lookupCost vx costs
          vd1 = ud + 1
       in if vd1 < vd0
            then (Q.insert vx vd1 () bq, M.insert vx (Step vd1 ux how) costs)
            else (bq, costs)

    recon :: HashMap s (Node s h) -> s -> ([s], [h])
    recon costs = walk [] []
      where
        walk accS accH k = case costs M.! k of
          Start        -> (k : accS, accH)
          Step _ k' h' -> walk (k : accS) (h' : accH) k'

lookupCost :: (Hashable s, Ord s) => s -> HashMap s (Node s h) -> Int
lookupCost k m = case M.lookup k m of
  Just Start           -> 0
  Just (Step cost _ _) -> cost
  Nothing              -> maxBound
