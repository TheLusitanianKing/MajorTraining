module GenerationSpec (spec) where


import Data.Set (Set)
import Data.Either (isLeft, isRight)
import Model
import Generation (generateCircuit)
import System.Random (StdGen, mkStdGen)
import Test.Hspec
import Test.HUnit
import Test.Hspec.QuickCheck (prop)

import qualified Data.Set as Set


spec :: Spec
spec = do
  describe "Circuit generation." $ do
    prop "Check it fails if there is no exercises to generate from." $ do
      \n -> isLeft (generateCircuit gen Set.empty n (circuitWithEmptySteps 3)) `shouldBe` True
    
    it "8 exercises to generate 3 rounds in a 3 steps circuit: should fail." $
      let
        littleSetOfExercises = Set.fromList . take 8 . Set.toList $ noEquipmentExercises
        generatedCircuit = generateCircuit gen littleSetOfExercises 3 (circuitWithEmptySteps 3)
      in isLeft generatedCircuit `shouldBe` True
    
    it "10 exercises to generate 3 rounds in a 3 steps circuit: should be fine."  $
      let
        littleSetOfExercises = Set.fromList . take 10 . Set.toList $ noEquipmentExercises
        generatedCircuit = generateCircuit gen littleSetOfExercises 3 (circuitWithEmptySteps 3)
      in isRight generatedCircuit `shouldBe` True
    
    describe "Generating X rounds on the Y steps circuit." $ do
      prop "X steps Y rounds = X*Y total exercises." $ do
        \x y -> 
          if x >= 0 && y >= 0
            then assertHaveXExercises (x*y) $ generateCircuit gen allExercises y (circuitWithEmptySteps x)
            else assertBool "" True -- TODO: see how to force x and y to be natural numbers

circuitWithEmptySteps :: Int -> Circuit
circuitWithEmptySteps nbSteps = Circuit
  { _circuitSteps = replicate nbSteps sampleStep } 
  where
    sampleStep = Step $ Set.fromList []

noEquipmentExercises :: Set Exercise
noEquipmentExercises = Set.filter (Set.null . _exerciseEquipments) allExercises

gen :: StdGen
gen = mkStdGen 1

assertHaveXExercises :: HasCallStack => Int -> Either a GeneratedCircuit -> Assertion
assertHaveXExercises x egc = case egc of
  Left _ -> assertBool [] True -- TODO: there's probably a better way
  Right gc ->
    assertEqual "Does not have the right number of exercises." x $
      nbPickedExercises gc 