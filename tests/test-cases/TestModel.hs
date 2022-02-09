module TestModel where


import Model
import Test.Hspec

import qualified Data.List.NonEmpty as NE


modelTests :: Spec
modelTests = do
  describe "Getting number of exercises inside a generated circuit." $ do
    it "Has four" $ do
      let x = nbPickedExercises $
                GeneratedCircuit sampleCircuit
                (NE.fromList [NE.fromList [sampleExercise, sampleExercise], NE.fromList [sampleExercise, sampleExercise]])
      x `shouldBe` 4
    
    it "Has two" $ do
      let x = nbPickedExercises $
                GeneratedCircuit sampleCircuit
                (NE.fromList [NE.fromList [sampleExercise], NE.fromList [sampleExercise]])
      x `shouldBe` 2

sampleCircuit = undefined
sampleExercise = undefined