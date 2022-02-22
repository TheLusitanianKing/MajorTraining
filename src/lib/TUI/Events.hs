module TUI.Events (appEvent) where


import Data.Maybe (fromMaybe)
import Model (Circuit(..), Equipment(..), Step(..))
import TUI.AppState (AppState(..), Name, maxNumberOfSteps)

import qualified Brick.Focus  as F
import qualified Brick.Main   as M
import qualified Brick.Types  as T
import qualified Data.Set     as Set
import qualified Graphics.Vty as V


appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc []        -> interruptEvent st
    V.EvKey (V.KChar ' ') [] -> generateEvent st
    V.EvKey V.KLeft []       -> prevStepEvent st
    V.EvKey V.KRight []      -> nextStepEvent st
    V.EvKey V.KUp []         -> prevEquipmentEvent st
    V.EvKey V.KDown []       -> nextEquipmentEvent st
    V.EvKey (V.KChar '+') [] -> addStepEvent st
    V.EvKey (V.KChar '-') [] -> removeStepEvent st
    V.EvKey V.KEnter []      -> selectEvent st
    _                        -> M.continue st
appEvent st _ = M.continue st

type EventHandler = AppState -> T.EventM Name (T.Next AppState)

interruptEvent :: EventHandler
interruptEvent st = M.halt $ st { _apsInterrupted = True }

generateEvent :: EventHandler
generateEvent = M.halt

prevEquipmentEvent :: EventHandler
prevEquipmentEvent st = M.continue $
  st { _apsFocusedEquipment = F.focusPrev (_apsFocusedEquipment st) }

nextEquipmentEvent :: EventHandler
nextEquipmentEvent st = M.continue $
  st { _apsFocusedEquipment = F.focusNext (_apsFocusedEquipment st) }

prevStepEvent :: EventHandler
prevStepEvent st = M.continue $
  st { _apsFocusedStepIndex = F.focusPrev (_apsFocusedStepIndex st) }

nextStepEvent :: EventHandler
nextStepEvent st = M.continue $
  st { _apsFocusedStepIndex = F.focusNext (_apsFocusedStepIndex st) }

-- TODO: to be improved with lenses
selectEvent :: EventHandler
selectEvent st = M.continue $
  let
    focusedEquipment = F.focusGetCurrent (_apsFocusedEquipment st)
    stepIndex = F.focusGetCurrent (_apsFocusedStepIndex st)
    circuit' :: Circuit -> Maybe Int -> Maybe Equipment -> Circuit
    circuit' original Nothing _ = original
    circuit' original _ Nothing = original
    circuit' original (Just i) (Just eq) =
      original { _circuitSteps = steps' (_circuitSteps original) i eq }
    steps' :: [Step] -> Int -> Equipment -> [Step]
    steps' steps i eq = helper [] steps 0
      where
        helper acc [] _ = reverse acc
        helper acc (x:xs) ci
          | i == ci   = helper (step' x eq:acc) xs (ci + 1)
          | otherwise = helper (x:acc) xs (ci + 1)
    step' :: Step -> Equipment -> Step
    step' step eq
      | eq `Set.member` _stepEquipments step =
        step { _stepEquipments = Set.delete eq (_stepEquipments step) }
      | otherwise =
        step { _stepEquipments = Set.insert eq (_stepEquipments step) }
  in st { _apsCircuit = circuit' (_apsCircuit st) stepIndex focusedEquipment }

addStepEvent :: EventHandler
addStepEvent st
  | length (_circuitSteps $ _apsCircuit st) + 1 > maxNumberOfSteps = M.continue st
  | otherwise = M.continue $
    let
      circuit = _apsCircuit st
      circuit' = circuit { _circuitSteps = _circuitSteps circuit ++ [Step Set.empty] }
      currentStep = fromMaybe 0 $ F.focusGetCurrent (_apsFocusedStepIndex st)
      nbSteps' = length $ _circuitSteps circuit'
    in st
        { _apsCircuit = circuit'
        , _apsFocusedStepIndex = F.focusSetCurrent currentStep $ F.focusRing [0..nbSteps' - 1]
        }

removeStepEvent :: EventHandler
removeStepEvent st
  | length (_circuitSteps $ _apsCircuit st) == 1 = M.continue st
  | otherwise = M.continue $
    let
      removeStepAt :: Int -> [a] -> [a]
      removeStepAt i xs = take i xs ++ drop (i+1) xs
      circuit = _apsCircuit st
      currentStep = fromMaybe 0 $ F.focusGetCurrent (_apsFocusedStepIndex st)
      circuit' = circuit { _circuitSteps = removeStepAt currentStep (_circuitSteps circuit) }
      nbSteps' = length $ _circuitSteps circuit'
    in st
        { _apsCircuit = circuit'
        , _apsFocusedStepIndex = F.focusRing [0..nbSteps' - 1]
        }