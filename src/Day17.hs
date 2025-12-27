module Day17 where

import Data.Attoparsec.ByteString.Char8 (Parser, char, decimal, endOfLine, parseOnly, sepBy, string)
import qualified Data.ByteString.Char8 as C
import qualified Data.IntSet as IntSet
import Data.Sequence ((|>))
import qualified Data.Text as T
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as VU
import Protolude

data Computer = Computer
  { regA :: !Int,
    regB :: !Int,
    regC :: !Int,
    code :: Vector Int,
    ip :: !Int,
    out :: Seq Int
  }
  deriving (Eq, Show)

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
      { regA = a,
        regB = b,
        regC = c,
        code = VU.fromList code',
        ip = 0,
        out = mempty
      }

solve' :: Computer -> (Text, Maybe Int)
solve' computer = (T.intercalate "," (map show (toList $ out (runComputer computer))), findA computer)

-- run the computer until it halts
runComputer :: Computer -> Computer
runComputer computer = maybe computer runComputer (tick computer)

findA :: Computer -> Maybe Int
findA computer = fst <$> IntSet.minView (go 0 0 mempty)
  where
    code' = code computer
    n = VU.length (code computer)
    go :: Int -> Int -> IntSet -> IntSet
    go a i results
      | output == code' = IntSet.insert a results
      | output == VU.slice (n - i) i code' || i == 0 = IntSet.unions [go (8 * a + j) (i + 1) results | j <- [0 .. 7]]
      | otherwise = results
      where
        computer' =
          Computer
            { regA = a,
              regB = regB computer,
              regC = regC computer,
              code = code computer,
              ip = ip computer,
              out = out computer
            }
        output = VU.fromList (toList (out (runComputer computer')))

tick :: Computer -> Maybe Computer
tick Computer {..} =
  let opcode = code VU.!? ip
      operand = code VU.! (ip + 1)
      evalCombo oper
        | oper >= 0 && oper <= 3 = Just oper
        | oper == 4 = Just regA
        | oper == 5 = Just regB
        | oper == 6 = Just regC
        | otherwise = Nothing
      toDouble :: Int -> Double
      toDouble = fromIntegral
   in case (opcode, evalCombo operand) of
        (Just 0, Just combo) ->
          Just
            Computer
              { regA = floor $ toDouble regA / toDouble ((2 :: Int) ^ combo),
                regB = regB,
                regC = regC,
                code = code,
                ip = ip + 2,
                out = out
              }
        (Just 1, _) ->
          Just
            Computer
              { regA = regA,
                regB = regB `xor` operand,
                regC = regC,
                code = code,
                ip = ip + 2,
                out = out
              }
        (Just 2, Just combo) ->
          Just
            Computer
              { regA = regA,
                regB = combo `mod` 8,
                regC = regC,
                code = code,
                ip = ip + 2,
                out = out
              }
        (Just 3, _) ->
          Just
            Computer
              { regA = regA,
                regB = regB,
                regC = regC,
                code = code,
                ip = if regA /= 0 then operand else ip + 2,
                out = out
              }
        (Just 4, _) ->
          Just
            Computer
              { regA = regA,
                regB = regB `xor` regC,
                regC = regC,
                code = code,
                ip = ip + 2,
                out = out
              }
        (Just 5, Just combo) ->
          Just
            Computer
              { regA = regA,
                regB = regB,
                regC = regC,
                code = code,
                ip = ip + 2,
                out = out |> (combo `mod` 8)
              }
        (Just 6, Just combo) ->
          Just
            Computer
              { regA = regA,
                regB = floor $ toDouble regA / toDouble ((2 :: Int) ^ combo),
                regC = regC,
                code = code,
                ip = ip + 2,
                out = out
              }
        (Just 7, Just combo) ->
          Just
            Computer
              { regA = regA,
                regB = regB,
                regC = floor $ toDouble regA / toDouble ((2 :: Int) ^ combo),
                code = code,
                ip = ip + 2,
                out = out
              }
        _ -> Nothing -- If the computer tries to read an opcode past the end of the program, it instead halts.
