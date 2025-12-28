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

type Matrix = HashMap Pos Char

type Pos = (Int, Int) -- row, col

solve :: C.ByteString -> Either Text (Int, Int)
solve input = solve' . buildMatrix <$> parseOnly inputParser input

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

buildMatrix :: [[Char]] -> (Matrix, Pos, Pos)
buildMatrix xs = foldl' f (mempty, (0, 0), (0, 0)) (zip [0 ..] (map (zip [0 ..]) xs))
  where
    f :: (Matrix, Pos, Pos) -> (Int, [(Int, Char)]) -> (Matrix, Pos, Pos)
    f acc (rowIdx, ys) = foldl' g acc ys
      where
        g :: (Matrix, Pos, Pos) -> (Int, Char) -> (Matrix, Pos, Pos)
        g (matrix, start, end) (colIdx, c) =
          let start' = if c == 'S' then (rowIdx, colIdx) else start
              end' = if c == 'E' then (rowIdx, colIdx) else end
           in (HashMap.insert (rowIdx, colIdx) c matrix, start', end')

solve' :: (Matrix, Pos, Pos) -> (Int, Int)
solve' (matrix, start, dest) =
  let parents = bfs start (map fst . neighbors matrix False)
      shortestLegalDist = Seq.length (reconstructPath parents dest) - 1
      allPaths = findAllPaths matrix start dest (shortestLegalDist - 100)
   in (length allPaths, 0)

neighbors :: Matrix -> Bool -> Pos -> [(Pos, Char)]
neighbors matrix cheating pos =
  let candidates = neighbors4 pos
      values = map (`HashMap.lookup` matrix) candidates

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

findAllPaths :: Matrix -> Pos -> Pos -> Int -> [(Int, [Pos])]
findAllPaths matrix start goal maxDist = dfs (DfsState {dfsStateCurrent = start, dfsStateDist = 0, dfsStateVisited = HashSet.singleton start, dfsStatePath = [start], dfsStateAllowCheat = True})
  where
    dfs :: DfsState -> [(Int, [Pos])]
    dfs DfsState {..}
      | dfsStateCurrent == goal && dfsStateDist <= maxDist = [(dfsStateDist, dfsStatePath)]
      | otherwise =
          let nexts = filter (\(x, _) -> dfsStateDist < maxDist && not (x `HashSet.member` dfsStateVisited)) (neighbors matrix dfsStateAllowCheat dfsStateCurrent)
           in concatMap (\(nb, val) -> dfs DfsState {dfsStateCurrent = nb, dfsStateDist = dfsStateDist + 1, dfsStateVisited = HashSet.insert nb dfsStateVisited, dfsStatePath = nb : dfsStatePath, dfsStateAllowCheat = dfsStateAllowCheat && val /= '#'}) nexts

cheats :: Matrix -> Pos -> [(Pos, Pos)]
cheats matrix pos =
  let candidates = concatMap (filter (\(_, q) -> q /= pos) . (\cand -> map (cand,) (neighbors4 cand))) (neighbors4 pos)
      uniqueCandidates = HashSet.toList (HashSet.fromList candidates)
   in -- we only care about pairs (p, q) :: (Pos, Pos) where p == '#' and q != '#'
      mapMaybe (\(p, q) -> if (HashMap.lookup p matrix, isEmpty <$> HashMap.lookup q matrix) == (Just '#', Just True) then Just (p, q) else Nothing) uniqueCandidates

isEmpty :: Char -> Bool
isEmpty '#' = False
isEmpty _ = True

-- Experimental Area
example :: (Matrix, Pos, Pos)
example = either (const (mempty, (0, 0), (0, 0))) buildMatrix (parseOnly inputParser exampleInput)

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
