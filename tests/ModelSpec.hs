module ModelSpec (spec) where


import Model (Circuit(..), Exercise, GeneratedCircuit(..), nbPickedExercises)
import Test.Hspec

import qualified Data.List.NonEmpty as NE


spec :: Spec
spec = do
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

sampleCircuit :: Circuit
sampleCircuit = undefined -- I don't even need to define this

sampleExercise :: Exercise 
sampleExercise = undefined -- or this, thanks to lazy evaluation