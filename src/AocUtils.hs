module AocUtils
  ( choose2,
    sortDesc,
    skipUntil,
    parseOnly,
    columns,
    neighbors4,
    neighbors8,
    digits,
    replicateInt,
    numDigits,
    bfs,
    dijkstra,
    reconstructPath,
  )
where

import Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.HashPSQ as PSQ
import qualified Data.HashSet as HashSet
import qualified Data.IntMap as IntMap
import Data.Sequence (Seq (..), (<|), (><))
import qualified Data.Sequence as Seq
import Protolude

-- | Generate all unique unordered pairs from a list.
-- Each pair (x, y) consists of two distinct elements from the input list,
-- with each combination appearing only once (order does not matter).
choose2 :: [a] -> [(a, a)]
choose2 xs = [(x, y) | (x : ys) <- tails xs, y <- ys]
{-# INLINE choose2 #-}

digits :: (Integral a) => a -> Seq a
digits n = go (abs n) empty
  where
    go 0 acc = if n == 0 then Seq.singleton 0 else acc
    go x acc = let (q, r) = x `quotRem` 10 in go q (r <| acc)
{-# INLINE digits #-}

-- | Repeats the integer 'a', 'n' times by concatenating its digits.
-- For example, replicateInt 12 3 == 121212
replicateInt :: (Integral a) => a -> Int -> a
replicateInt a n = foldl' (\acc _ -> acc * pow10b + a) a [2 .. n]
  where
    b = numDigits a
    pow10b = 10 ^ b
{-# INLINE replicateInt #-}

-- |
--  numDigits computes the number of digits in a given integer.
--  For example, numDigits 1234 == 4.
numDigits :: (Integral t) => t -> Int
numDigits n = Seq.length (digits n)
{-# INLINE numDigits #-}

sortDesc :: (Ord a) => [a] -> [a]
sortDesc = sortBy (comparing Down)
{-# INLINE sortDesc #-}

-- | Skip input until 'p' succeeds, then return its result.
skipUntil :: Parser a -> Parser a
skipUntil p = p <|> (AP.anyChar *> skipUntil p)

parseOnly :: Parser a -> ByteString -> Either Text a
parseOnly p = first toS . AP.parseOnly p

-- | Transpose a list of rows into a map of columns.
-- Each key in the resulting 'IntMap' is the column index,
-- and its value is a 'Seq' containing all elements from that column,
-- in the order they appeared in the input rows.
-- Handles ragged (non-square) input: missing values in shorter rows are ignored.
--
-- Example:
-- >>> columns [[1,2],[3,4,5]]
-- fromList [(0,fromList [1,3]), (1,fromList [2,4]), (2,fromList [5])]
columns :: [[a]] -> IntMap (Seq a)
columns xs = foldl' f mempty (map (zip [0 ..]) xs)
  where
    f = foldl' (\acc' (col, val) -> IntMap.insertWith (flip (><)) col (Seq.singleton val) acc')
{-# INLINE columns #-}

-- | Returns the 4 points orthogonally adjacent to the given point.
neighbors4 :: (Num a, Num b) => (a, b) -> [(a, b)]
neighbors4 (x, y) = [(x + 1, y), (x, y + 1), (x - 1, y), (x, y - 1)]
{-# INLINE neighbors4 #-}

-- | Returns the 8 points orthogonally or diagonally adjacent to the given point.
neighbors8 :: (Eq a, Eq b, Num a, Num b) => (a, b) -> [(a, b)]
neighbors8 (row, col) =
  [ (row - 1, col - 1),
    (row - 1, col),
    (row - 1, col + 1),
    (row, col - 1),
    (row, col + 1),
    (row + 1, col - 1),
    (row + 1, col),
    (row + 1, col + 1)
  ]
{-# INLINE neighbors8 #-}

-- | Performs a breadth-first search (BFS) starting from a given node.
-- Takes a starting node and a function that returns the list of neighbors for any node.
-- Returns a map from each reachable node to its parent in the BFS tree, representing the traversal path.
bfs :: (Hashable a) => a -> (a -> [a]) -> HashMap a a
bfs start neighbors = go (Seq.singleton start) HashMap.empty (HashSet.singleton start)
  where
    go Seq.Empty parents _ = parents
    go (current :<| queue) parents visited =
      let unvisited = filter (\nb -> not (HashSet.member nb visited)) (neighbors current)
          newParents = foldl' (\m n -> HashMap.insert n current m) parents unvisited -- mark current as parent of each unvisited node
          newVisited = foldl' (flip HashSet.insert) visited unvisited
          newQueue = queue <> Seq.fromList unvisited
       in go newQueue newParents newVisited
{-# INLINE bfs #-}

type Weight = Int

dijkstra :: (Eq a, Ord a, Hashable a) => a -> (a -> [(a, Weight)]) -> a -> Maybe Int
dijkstra start neighbors goal = go (PSQ.singleton start 0 ()) (HashMap.singleton start 0)
  where
    go pq dist = case PSQ.minView pq of
      Nothing -> Nothing
      Just (s, c, _, pq')
        | s == goal -> Just c
        | otherwise -> go pq'' dist'
        where
          relevant = [(n, cc) | (n, w) <- neighbors s, let cc = c + w, cc `less` (dist HashMap.!? n)] -- only visit neighbors if path is shorter than the previously known one
          pq'' = foldr (\(n, cc) pqAcc -> update n cc pqAcc) pq' relevant -- update priority queue with potentially shorter distances to the neighbors
          dist' = HashMap.fromList [(n, cc) | (n, cc) <- relevant] `HashMap.union` dist -- If a key occurs in both maps, the mapping from the first wins
    update n c queue = snd $ PSQ.alter upsert n queue
      where
        upsert Nothing = ((), Just (c, ()))
        upsert (Just (c', _)) = ((), Just (min c c', ()))

    less :: (Ord a) => a -> Maybe a -> Bool
    x `less` may = maybe True (x <) may
{-# INLINE dijkstra #-}

-- | Constructs the path from the start node to the given `goal` node using a map of predecessors.
-- The predecessor map should associate each node with its immediate predecessor on the path.
-- Returns the list of nodes representing the path from start to goal, in order.
reconstructPath :: (Eq a, Hashable a) => HashMap a a -> a -> Seq a
reconstructPath parents goal = go goal mempty
  where
    go node acc =
      case HashMap.lookup node parents of
        Nothing -> node <| acc
        Just parent -> go parent (node <| acc)
{-# INLINE reconstructPath #-}
