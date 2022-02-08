import Test.Hspec
import Test.QuickCheck
import TestGeneration (generationTests)

main :: IO ()
main = hspec $ do
  generationTests