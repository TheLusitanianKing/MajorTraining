module TestGeneration where

import Generation (generateCircuit)
import Test.Hspec

generationTests :: Spec
generationTests = do
  describe "A..." $ do
    it "B..." $ do
      True `shouldBe` True