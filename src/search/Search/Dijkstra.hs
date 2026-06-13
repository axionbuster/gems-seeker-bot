-- | Generic uniform-cost (Dijkstra) search.
--
-- TODO: port the efficient priority-search-queue implementation from
-- references/pure-solver (@src/Dijk.hs@). This stub typechecks the interface
-- but finds nothing; tests will drive the real implementation. Performance
-- matters here.
module Search.Dijkstra
  ( dijkstra
  ) where

-- | Lowest-cost path from a start state to any goal state.
--
-- @successors@ yields each neighbour with its non-negative step cost. Returns
-- the path (inclusive of start and goal) if a goal is reachable.
dijkstra
  :: Ord s
  => s                    -- ^ start state
  -> (s -> Bool)          -- ^ goal predicate
  -> (s -> [(s, Int)])    -- ^ successors with step costs
  -> Maybe [s]
dijkstra _start _isGoal _successors = Nothing
