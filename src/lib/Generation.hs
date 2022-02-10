module Generation 
  ( generateCircuit
  )
where


import Data.Set (Set)
import Model (Circuit(..), Exercise(..), GeneratedCircuit(..), Step (_stepEquipments))
import System.Random (Random(randomR), StdGen)

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set


-- | Generate a circuit from its steps, a set of exercises to use, a number of round to generate and a generator
generateCircuit :: StdGen -> Set Exercise -> Int -> Circuit -> Either String GeneratedCircuit
generateCircuit gen exs nbRounds c
  | null sts = Left "Empty circuit given."
  | otherwise =
    helper [] [] exs sts gen nbRounds
    where
      sts = _circuitSteps c
      helper acc tmp es steps g n
        | n == 0      =
          case acc of
            [] -> Left "No rounds generated."
            _  -> Right $ GeneratedCircuit { _circuit = c, _rounds = NE.fromList (reverse acc) }
        | Set.null es = Left "Not enough exercises to generate the circuit."
        | null steps  = helper (NE.fromList (reverse tmp):acc) [] es (_circuitSteps c) g (n - 1)
        | otherwise   =
          let
            currentStep = head steps
            validExercises = Set.filter (stepCanHaveThisExercise currentStep) es
            nbExs = length validExercises
          in -- TODO: avoid that ugly staircase
            case nbExs of
              0 -> Left "No more valid exercises for the step."
              _ ->
                let
                  (i, g') = randomR (0, nbExs - 1) g
                  pickedExercise = Set.toList validExercises !! i
                  remExs = es `Set.difference` Set.fromList [pickedExercise]
                in helper acc (pickedExercise:tmp) remExs (tail steps) g' n

-- | Check if the given step can have the given exercise assigned to it
stepCanHaveThisExercise :: Step -> Exercise -> Bool
stepCanHaveThisExercise s e = _exerciseEquipments e `Set.isSubsetOf` _stepEquipments s