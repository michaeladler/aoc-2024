module Main where

import qualified Data.ByteString.Char8 as C
import qualified Day17
import qualified Day20
import Protolude

main :: IO ()
main =
  getArgs >>= \args ->
    case mapMaybe readMaybe args of
      days@(_x : _xs) -> mapM_ runDay days
      _ -> mapM_ runDay [17 .. 25]

runDay :: Int -> IO ()
runDay 17 = runSolver Day17.solve "input/17.txt"
runDay 20 = runSolver Day20.solve "input/20.txt"
runDay _ = putText "Day not implemented yet."

runSolver :: (Show a) => (C.ByteString -> Either Text a) -> FilePath -> IO ()
runSolver solver fileName = C.readFile fileName >>= \content -> either putErrText print (solver content)
