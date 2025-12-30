module Day17 where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, parseOnly, sepBy, string)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet as IntSet
import Data.Sequence ((|>))
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Lens.Micro
import Lens.Micro.TH
import Protolude

data Computer = Computer
  { _regA :: !Int,
    _regB :: !Int,
    _regC :: !Int,
    _code :: Vector Int,
    _ip :: !Int,
    _out :: Seq Int
  }

makeLenses ''Computer

solve :: C.ByteString -> Either Text (Text, Maybe Int)
solve input = solve' <$> first toS (parseOnly inputParser input)

inputParser :: Parser Computer
inputParser = do
  a <- string "Register A: " *> decimal <* endOfLine
  b <- string "Register B: " *> decimal <* endOfLine
  c <- string "Register C: " *> decimal <* endOfLine
  _ <- endOfLine *> string "Program: "
  code' <- decimal `sepBy` char ','
  return
    Computer
      { _regA = a,
        _regB = b,
        _regC = c,
        _code = VU.fromList code',
        _ip = 0,
        _out = mempty
      }

solve' :: Computer -> (Text, Maybe Int)
solve' computer = (T.intercalate "," (map show (toList $ runComputer computer ^. out)), findA computer)

-- run the computer until it halts
runComputer :: Computer -> Computer
runComputer computer = maybe computer runComputer (tick computer)

findA :: Computer -> Maybe Int
findA computer = fst <$> IntSet.minView (go 0 0 mempty)
  where
    code' = computer ^. code
    n = VU.length code'
    go :: Int -> Int -> IntSet -> IntSet
    go a i results
      | output == code' = IntSet.insert a results
      | output == VU.slice (n - i) i code' || i == 0 = IntSet.unions [go (8 * a + j) (i + 1) results | j <- [0 .. 7]]
      | otherwise = results
      where
        computer' = computer & regA .~ a
        output = VU.fromList (toList (runComputer computer' ^. out))

tick :: Computer -> Maybe Computer
tick computer =
  let opcode = (computer ^. code) VU.!? (computer ^. ip)
      operand = (computer ^. code) VU.! (computer ^. ip + 1)
      evalCombo oper
        | oper >= 0 && oper <= 3 = Just oper
        | oper == 4 = Just (computer ^. regA)
        | oper == 5 = Just (computer ^. regB)
        | oper == 6 = Just (computer ^. regC)
        | otherwise = Nothing
      toDouble :: Int -> Double
      toDouble = fromIntegral
   in case (opcode, evalCombo operand) of
        (Just 0, Just combo) ->
          Just $
            computer
              & (regA %~ (\a' -> floor (toDouble a' / toDouble (2 ^ combo))))
              & (ip %~ (+ 2))
        (Just 1, _) ->
          Just $
            computer
              & regB %~ (`xor` operand)
              & ip %~ (+ 2)
        (Just 2, Just combo) ->
          Just $
            computer
              & regB .~ (combo `mod` 8)
              & ip %~ (+ 2)
        (Just 3, _) -> Just (computer & ip %~ (\ip' -> if (computer ^. regA) /= 0 then operand else ip' + 2))
        (Just 4, _) ->
          Just $
            computer
              & regB %~ (`xor` (computer ^. regC))
              & ip %~ (+ 2)
        (Just 5, Just combo) ->
          Just $
            computer
              & out %~ (|> (combo `mod` 8))
              & ip %~ (+ 2)
        (Just 6, Just combo) ->
          Just $
            computer
              & regB .~ floor (toDouble (computer ^. regA) / toDouble (2 ^ combo))
              & ip %~ (+ 2)
        (Just 7, Just combo) ->
          Just $
            computer
              & regC .~ floor (toDouble (computer ^. regA) / toDouble (2 ^ combo))
              & ip %~ (+ 2)
        _ -> Nothing -- If the computer tries to read an opcode past the end of the program, it instead halts.
