{-# LANGUAGE OverloadedStrings #-}

module TUI.Rendering where


import Brick.Widgets.Core
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Model
import TUI.AppState

import qualified Brick.AttrMap              as A
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Focus                as F
import qualified Brick.Types                as T
import qualified Data.Set                   as Set


drawUI :: AppState -> [T.Widget Name]
drawUI as = [ui]
  where
    focusedEquipment = F.focusGetCurrent (_apsFocusedEquipment as)
    focusedStep = fromMaybe 0 $ F.focusGetCurrent (_apsFocusedStepIndex as)
    steps =
      zipWith (\index step -> drawStep index step focusedEquipment (index == focusedStep)) [0..]
      . _circuitSteps
      . _apsCircuit
      $ as
    ui = C.vCenter $ C.hCenter $ hBox steps <=> padTop (T.Pad 2) helperKeys

helperKeys :: T.Widget Name
helperKeys =
  vBox
    [ keyWidget "key"          "Up-Down"    "Navigate between the equipments"
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

drawStep :: Int -> Step -> Maybe Equipment -> Bool -> T.Widget Name
drawStep index step focusedEquipment isFocusedStep =
  let
    equipments :: Set Equipment
    equipments = _stepEquipments step
    selection :: [(Equipment, Bool)]
    selection = map (\e -> (e, e `Set.member` equipments)) allEquipments
  in
    withBorderStyle BS.ascii $
    B.borderWithLabel (str $ "Step " <> show (index + 1)) $
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