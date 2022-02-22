{-# LANGUAGE OverloadedStrings #-}

module TUI where


import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Model
import Brick.Widgets.Core

import qualified Brick.AttrMap              as A
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Focus                as F
import qualified Brick.Main                 as M
import qualified Brick.Types                as T
import qualified Brick.Util                 as U
import qualified Graphics.Vty               as V

import qualified Data.Set as Set


data AppState = AppState
  { _apsCircuit          :: Circuit
  , _apsFocusedStepIndex :: F.FocusRing Int
  , _apsFocusedEquipment :: F.FocusRing Equipment
  , _apsInterrupted      :: Bool
  }

instance Show AppState where
  show st = "AppState Circuit: " <> show (_apsCircuit st)

initialAppState :: AppState
initialAppState = AppState
  { _apsCircuit          = circuit
  , _apsFocusedStepIndex = F.focusRing [0..nbSteps - 1]
  , _apsFocusedEquipment = F.focusRing allEquipments
  , _apsInterrupted      = False -- if true, does not need to generate the circuit
  }
  where
    circuit = Circuit [Step Set.empty, Step Set.empty]
    nbSteps = length . _circuitSteps $ circuit

type Name = () -- might change

app :: M.App AppState e Name
app = M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attrMap
  }

drawUI :: AppState -> [T.Widget Name]
drawUI as = [ui]
  where
    focusedEquipment = F.focusGetCurrent (_apsFocusedEquipment as)
    focusedStep = fromMaybe 0 $ F.focusGetCurrent (_apsFocusedStepIndex as)
    steps =
      zipWith (\index step -> drawStep step focusedEquipment (index == focusedStep)) [0..]
      . _circuitSteps
      . _apsCircuit
      $ as
    ui =
      C.vCenter $
      C.hCenter $
        hBox steps <=> padTop (T.Pad 2) helperKeys

helperKeys :: T.Widget Name
helperKeys =
  vBox
    [ keyWidget "key"          "Up-down"    "Navigate between the equipments"
    , keyWidget "key"          "Left-Right" "Navigate between the steps"
    , keyWidget "key"          "Enter"      "Select/deselect the focused equipment"
    , keyWidget "key"          "+"          "Add a step"
    , keyWidget "key"          "-"          "Remove the currently focused step"
    , keyWidget "importantKey" "Space"      "Start the generation"
    , keyWidget "dangerousKey" "Esc"        "Quit"
    ]
  where
    keyWidget :: A.AttrName -> Text -> Text -> T.Widget Name
    keyWidget keyAttr keyName keyDescription =
      withAttr keyAttr (txt keyName) <+> txt (": " <> keyDescription)

drawStep :: Step -> Maybe Equipment -> Bool -> T.Widget Name
drawStep step focusedEquipment isFocusedStep =
  let
    equipments :: Set Equipment
    equipments = _stepEquipments step
    selection :: [(Equipment, Bool)]
    selection = map (\e -> (e, e `Set.member` equipments)) allEquipments
  in
  withBorderStyle BS.ascii $
  B.borderWithLabel (str "Step") $
  vLimitPercent 30 $
  C.vCenter $
  vBox $
    str "Equipments: " : map (drawSelection isFocusedStep focusedEquipment) selection

drawSelection :: Bool -> Maybe Equipment -> (Equipment, Bool) -> T.Widget Name
drawSelection True (Just focusedEquipment) (e, selected) =
  drawEquipment e selected (focusedEquipment == e)
drawSelection _ _ (e, selected) = drawEquipment e selected False

drawEquipment :: Equipment -> Bool -> Bool -> T.Widget Name
drawEquipment equipment isSelected isFocused =
  let
    icone = if isSelected then "✓" else "✕"
    attrIcone = if isSelected then "success" else "failure"
    equipmentWidget =
      if isFocused 
        then withAttr "focused" . str $ show equipment
        else str $ show equipment
  in
  padLeftRight 2 $
  withAttr attrIcone (str icone) <+> padLeft (T.Pad 2) equipmentWidget

-- TODO: lenses
appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt $ st { _apsInterrupted = True }
    V.EvKey (V.KChar ' ') [] -> M.halt st
    V.EvKey V.KLeft [] ->
      M.continue $
        st { _apsFocusedStepIndex = F.focusPrev (_apsFocusedStepIndex st) }
    V.EvKey V.KRight  [] ->
      M.continue $
        st { _apsFocusedStepIndex = F.focusNext (_apsFocusedStepIndex st) }
    V.EvKey V.KUp [] ->
      M.continue $
        st { _apsFocusedEquipment = F.focusPrev (_apsFocusedEquipment st) }
    V.EvKey V.KDown [] ->
      M.continue $
        st { _apsFocusedEquipment = F.focusNext (_apsFocusedEquipment st) }
    V.EvKey V.KEnter [] ->
      M.continue $
        -- TODO: I really need to learn to use lenses..
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
    _ -> M.continue st
appEvent st _ = M.continue st

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
  [ ("success", U.fg V.green)
  , ("failure", U.fg V.red)
  , ("key", V.white `U.on` V.blue)
  , ("importantKey", V.white `U.on` V.green)
  , ("dangerousKey", V.white `U.on` V.red)
  , ("focused", U.fg V.brightBlue)
  ]