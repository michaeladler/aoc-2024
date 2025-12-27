module Day17Spec (spec) where

import qualified Data.ByteString.Char8 as C
import Day17 (solve)
import Protolude
import System.Directory (doesFileExist)
import Test.Hspec

spec :: Spec
spec = do
  describe "solve" $ do
    it "should solve example 1" $ do
      fst <$> solve exampleInput `shouldBe` Right "4,6,3,5,6,3,5,2,1,0"
    it "should solve example 2" $ do
      snd <$> solve exampleInput2 `shouldBe` Right (Just 117440)
    it "should solve the actual problem" $ do
      maybeInput <- readMyInput "input/17.txt"
      case maybeInput of
        Nothing -> pendingWith "input file missing"
        Just input' -> solve input' `shouldBe` Right ("2,1,0,1,7,2,5,0,3", Just 267265166222235)
  where
    exampleInput =
      """
      Register A: 729
      Register B: 0
      Register C: 0

      Program: 0,1,5,4,3,0
      """
    exampleInput2 =
      """
      Register A: 2024
      Register B: 0
      Register C: 0

      Program: 0,3,5,4,3,0
      """

readMyInput :: FilePath -> IO (Maybe ByteString)
readMyInput fp = do
  exists <- doesFileExist fp
  if exists then Just <$> C.readFile fp else return Nothing
