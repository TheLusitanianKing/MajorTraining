module TestGeneration where

import Data.Set (Set)
import Data.Either (isLeft, isRight)
import Model (Circuit(..), Step(..), Exercise(..), allExercises)
import Generation (generateCircuit)
import System.Random (StdGen, mkStdGen)
import Test.Hspec (Spec, describe, it, pending, shouldBe)
import Test.Hspec.QuickCheck (prop)

import qualified Data.Set as Set

generationTests :: Spec
generationTests = do
  describe "Circuit generation." $ do
    prop "Check it fails if there is no exercises to generate from." $ do
      \n -> isLeft (generateCircuit gen Set.empty n threeStepWithoutEquipmentsCircuit) `shouldBe` True
    
    it "8 exercises to generate 3 rounds in a 3 steps circuit: should fail." $
      let
        littleSetOfExercises = Set.fromList . take 8 . Set.toList $ noEquipmentExercises
        generatedCircuit = generateCircuit gen littleSetOfExercises 3 threeStepWithoutEquipmentsCircuit
      in isLeft generatedCircuit `shouldBe` True
    
    it "10 exercises to generate 3 rounds in a 3 steps circuit: should be fine."  $
      let
        littleSetOfExercises = Set.fromList . take 10 . Set.toList $ noEquipmentExercises
        generatedCircuit = generateCircuit gen littleSetOfExercises 3 threeStepWithoutEquipmentsCircuit
      in isRight generatedCircuit `shouldBe` True
    
    describe "Generating X rounds on the Y steps circuit." $ do
      it "X steps Y rounds = X*Y total exercises." $ do
        pending
      
      it "X steps Y rounds = X exercises x Y rounds" $ do
        pending

threeStepWithoutEquipmentsCircuit :: Circuit
threeStepWithoutEquipmentsCircuit = Circuit
  { _circuitSteps = [step1, step2, step3] }
  where
    step1 = Step $ Set.fromList []
    step2 = Step $ Set.fromList []
    step3 = Step $ Set.fromList []

noEquipmentExercises :: Set Exercise
noEquipmentExercises = Set.filter (Set.null . _exerciseEquipments) allExercises

gen :: StdGen
gen = mkStdGen 1