import Test.Hspec (hspec)
import TestGeneration (generationTests)

main :: IO ()
main = hspec $ do
  generationTests