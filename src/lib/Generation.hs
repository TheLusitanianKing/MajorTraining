module Generation 
  ( generateCircuit
  )
where


import Data.Set (Set)
import Model (Circuit(..), Exercise(..), GeneratedCircuit(..))
import System.Random (Random(randomR), StdGen)

import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set


generateCircuit :: StdGen -> Set Exercise -> Int -> Circuit -> Either String GeneratedCircuit
generateCircuit gen exs nbRounds c =
  helper [] [] exs (circuitSteps c) gen nbRounds
  where
    helper acc tmp es steps g n
      | n == 0      =
        case acc of
          [] -> Left "No rounds generated."
          _  -> Right $ GeneratedCircuit { _circuit = c, _rounds = NE.fromList (reverse acc) }
      | Set.null es = Left "Not enough exercises to generate the circuit."
      | null steps  = helper (NE.fromList (reverse tmp):acc) [] es (circuitSteps c) g (n - 1)
      | otherwise   =
        let
          validExercises = Set.filter undefined es
          nbExs = length validExercises
          (i, g') = randomR (0, nbExs) g
          pickedExercise = Set.toList validExercises !! i
          remExs = es `Set.difference` Set.fromList [pickedExercise]
        in helper acc (pickedExercise:tmp) remExs (tail steps) g' n