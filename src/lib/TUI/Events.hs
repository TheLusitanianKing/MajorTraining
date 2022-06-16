module TUI.Events (appEvent) where


import Control.Lens (over, set, view)
import Data.Maybe (fromMaybe)
import Model (Circuit(..), Equipment(..), Step(..), circuitSteps, stepEquipments)
import TUI.AppState (AppState, Name, apsCircuit, apsFocusedEquipment, apsFocusedStepIndex, apsInterrupted, validNumberOfSteps)

import qualified Brick.Focus  as F
import qualified Brick.Main   as M
import qualified Brick.Types  as T
import qualified Data.Set     as Set
import qualified Graphics.Vty as V


-- | The main event handler function for the brick app
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

-- | When the user quits the app
interruptEvent :: EventHandler
interruptEvent = M.halt . set apsInterrupted True

-- | When the user wants to generate the circuit training
generateEvent :: EventHandler
generateEvent = M.halt

-- | When the user wants to go to the previous equipment
prevEquipmentEvent :: EventHandler
prevEquipmentEvent = M.continue . over apsFocusedEquipment F.focusPrev

-- | When the user wants to go to the next equipment
nextEquipmentEvent :: EventHandler
nextEquipmentEvent = M.continue . over apsFocusedEquipment F.focusNext

-- | When the user wants to go to the previous step
prevStepEvent :: EventHandler
prevStepEvent = M.continue . over apsFocusedStepIndex F.focusPrev

-- | When the user wants to go to the next step
nextStepEvent :: EventHandler
nextStepEvent = M.continue . over apsFocusedStepIndex F.focusNext

-- | When the user selects (or deselects) the currently focused equipment
--   of the currently focused step
selectEvent :: EventHandler
selectEvent st =
  M.continue $ over apsCircuit (circuit' stepIndex focusedEquipment) st
  where
    focusedEquipment = F.focusGetCurrent $ view apsFocusedEquipment st
    stepIndex = F.focusGetCurrent $ view apsFocusedStepIndex st
    circuit' :: Maybe Int -> Maybe Equipment -> Circuit -> Circuit
    circuit' Nothing _ original = original
    circuit' _ Nothing original = original
    circuit' (Just i) (Just eq) original =
      over circuitSteps (steps' i eq) original
    steps' :: Int -> Equipment -> [Step] -> [Step]
    steps' i eq steps = helper [] steps 0
      where
        helper acc [] _ = reverse acc
        helper acc (x:xs) ci
          | i == ci   = helper (step' x eq:acc) xs (ci + 1)
          | otherwise = helper (x:acc) xs (ci + 1)
    step' :: Step -> Equipment -> Step
    step' step eq
      | eq `Set.member` view stepEquipments step =
        over stepEquipments (Set.delete eq) step
      | otherwise = over stepEquipments (Set.insert eq) step

-- | When the user wants to add a new step
addStepEvent :: EventHandler
addStepEvent st
  | not . validNumberOfSteps $ length (view (apsCircuit . circuitSteps) st) + 1 =
    M.continue st
  | otherwise = M.continue $
    let
      circuit' = over circuitSteps (++ [Step Set.empty]) $ view apsCircuit st
      nbSteps' = length $ view circuitSteps circuit'
    in
      -- TODO: could using some kind of traversal avoid me using this weird function
      -- as both modified fields are dependant from each other
      over apsFocusedStepIndex ((\cs -> F.focusSetCurrent cs $ F.focusRing [0..nbSteps' - 1]) . fromMaybe 0 . F.focusGetCurrent) .
      set apsCircuit circuit' $ st

-- | When the user wants to remove the currently focused step
removeStepEvent :: EventHandler
removeStepEvent st
  | length (view (apsCircuit . circuitSteps) st) == 1 = M.continue st
  | otherwise = M.continue $
    let
      removeStepAt :: Int -> [a] -> [a]
      removeStepAt i xs = take i xs ++ drop (i+1) xs
      currentStep = fromMaybe 0 $ F.focusGetCurrent (view apsFocusedStepIndex st)
      circuit' = over circuitSteps (removeStepAt currentStep) $ view apsCircuit st
      nbSteps' = length $ view circuitSteps circuit'
    in
      -- TODO: same than above: both set operation are dependant and the way I did it is really not great
      set apsFocusedStepIndex (F.focusRing [0..nbSteps' - 1]) . set apsCircuit circuit' $ st