module Day20 where

import AocUtils
import Control.Monad (fail)
import Data.Attoparsec.ByteString.Char8 (Parser, anyChar, endOfLine, many1, sepBy)
import qualified Data.ByteString.Char8 as C
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.Sequence as Seq
import Protolude hiding (state)

type Grid = HashMap Pos Char

type Pos = (Int, Int) -- row, col

solve :: C.ByteString -> Either Text (Int, Int)
solve input = solve' . buildGrid <$> parseOnly inputParser input

inputParser :: Parser [[Char]]
inputParser = lineParser `sepBy` endOfLine
  where
    lineParser :: Parser [Char]
    lineParser = many1 cellParser

    cellParser :: Parser Char
    cellParser =
      anyChar >>= \c -> case c of
        '.' -> return c
        '#' -> return c
        'S' -> return c
        'E' -> return c
        _ -> fail "Invalid input"

buildGrid :: [[Char]] -> (Grid, Pos, Pos)
buildGrid xs = foldl' f (mempty, (0, 0), (0, 0)) (zip [0 ..] (map (zip [0 ..]) xs))
  where
    f :: (Grid, Pos, Pos) -> (Int, [(Int, Char)]) -> (Grid, Pos, Pos)
    f acc (rowIdx, ys) = foldl' g acc ys
      where
        g :: (Grid, Pos, Pos) -> (Int, Char) -> (Grid, Pos, Pos)
        g (grid, start, end) (colIdx, c) =
          let start' = if c == 'S' then (rowIdx, colIdx) else start
              end' = if c == 'E' then (rowIdx, colIdx) else end
           in (HashMap.insert (rowIdx, colIdx) c grid, start', end')

solve' :: (Grid, Pos, Pos) -> (Int, Int)
solve' (grid, start, dest) =
  let parents = bfs start (map fst . neighbors grid False)
      shortestLegalDist = Seq.length (reconstructPath parents dest) - 1
      allPaths = findAllPaths grid start dest (shortestLegalDist - 100)
   in (length allPaths, 0)

neighbors :: Grid -> Bool -> Pos -> [(Pos, Char)]
neighbors grid cheating pos =
  let candidates = neighbors4 pos
      values = map (`HashMap.lookup` grid) candidates

      f cand (Just val) = if cheating || isEmpty val then Just (cand, val) else Nothing
      f _ Nothing = Nothing
      zipped = catMaybes (zipWith f candidates values)
   in zipped

data DfsState = DfsState
  { dfsStateCurrent :: !Pos,
    dfsStateDist :: !Int,
    dfsStateVisited :: HashSet Pos,
    dfsStatePath :: [Pos],
    dfsStateAllowCheat :: !Bool
  }
  deriving (Eq, Show)

findAllPaths :: Grid -> Pos -> Pos -> Int -> [(Int, [Pos])]
findAllPaths grid start goal maxDist = dfs (DfsState {dfsStateCurrent = start, dfsStateDist = 0, dfsStateVisited = HashSet.singleton start, dfsStatePath = [start], dfsStateAllowCheat = True})
  where
    dfs :: DfsState -> [(Int, [Pos])]
    dfs DfsState {..}
      | dfsStateCurrent == goal && dfsStateDist <= maxDist = [(dfsStateDist, dfsStatePath)]
      | otherwise =
          let nexts = filter (\(x, _) -> dfsStateDist < maxDist && not (x `HashSet.member` dfsStateVisited)) (neighbors grid dfsStateAllowCheat dfsStateCurrent)
           in concatMap (\(nb, val) -> dfs DfsState {dfsStateCurrent = nb, dfsStateDist = dfsStateDist + 1, dfsStateVisited = HashSet.insert nb dfsStateVisited, dfsStatePath = nb : dfsStatePath, dfsStateAllowCheat = dfsStateAllowCheat && val /= '#'}) nexts

cheats :: Grid -> Pos -> [(Pos, Pos)]
cheats grid pos =
  let candidates = concatMap (filter (\(_, q) -> q /= pos) . (\cand -> map (cand,) (neighbors4 cand))) (neighbors4 pos)
      uniqueCandidates = HashSet.toList (HashSet.fromList candidates)
   in -- we only care about pairs (p, q) :: (Pos, Pos) where p == '#' and q != '#'
      mapMaybe (\(p, q) -> if (HashMap.lookup p grid, isEmpty <$> HashMap.lookup q grid) == (Just '#', Just True) then Just (p, q) else Nothing) uniqueCandidates

isEmpty :: Char -> Bool
isEmpty '#' = False
isEmpty _ = True

-- Experimental Area
example :: (Grid, Pos, Pos)
example = either (const (mempty, (0, 0), (0, 0))) buildGrid (parseOnly inputParser exampleInput)

exampleInput :: C.ByteString
exampleInput =
  """
  ###############
  #...#...#.....#
  #.#.#.#.#.###.#
  #S#...#.#.#...#
  #######.#.#.###
  #######.#.#...#
  #######.#.###.#
  ###..E#...#...#
  ###.#######.###
  #...###...#...#
  #.#####.#.###.#
  #.#...#.#.#...#
  #.#.#.#.#.#.###
  #...#...#...###
  ###############
  """
