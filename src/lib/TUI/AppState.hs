module TUI.AppState
  ( AppState(..)
  , Name
  , initialAppState
  , validNumberOfSteps
  )
where


import Model (Circuit(..), Equipment(..), Step(..),  allEquipments)

import qualified Brick.Focus as F
import qualified Data.Set as Set


type Name = () -- might change

-- | The main state for brick
data AppState = AppState
  { _apsCircuit          :: Circuit
  , _apsFocusedStepIndex :: F.FocusRing Int
  , _apsFocusedEquipment :: F.FocusRing Equipment
  , _apsInterrupted      :: Bool
  }

instance Show AppState where
  show st = "AppState Circuit: " <> show (_apsCircuit st)

-- | The state with which we start
initialAppState :: Int -> AppState
initialAppState initialNumberOfSteps = AppState
  { _apsCircuit          = circuit
  , _apsFocusedStepIndex = F.focusRing [0..nbSteps - 1]
  , _apsFocusedEquipment = F.focusRing allEquipments
  , _apsInterrupted      = False -- if true, does not generate the circuit when existing
  }
  where
    circuit = Circuit $ replicate initialNumberOfSteps (Step Set.empty)
    nbSteps = length . _circuitSteps $ circuit

-- | The maximum number of steps the circuit can have
--   only present for rendering limits
-- TODO: might be nice to avoid such limits, modifying the way the steps are presented now
validNumberOfSteps :: Int -> Bool
validNumberOfSteps x = 1 <= x && x <= 5