import Test.Hspec (hspec)
import TestGeneration (generationTests)
import TestModel (modelTests)


main :: IO ()
main = hspec $ do
  modelTests
  generationTests