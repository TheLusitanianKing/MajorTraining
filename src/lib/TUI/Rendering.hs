{-# LANGUAGE OverloadedStrings #-}

module TUI.Rendering (drawUI) where


import Brick.Widgets.Core
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Model (Circuit(..), Equipment(..), Step(..), allEquipments)
import TUI.AppState (AppState(..), Name)

import qualified Brick.AttrMap              as A
import qualified Brick.Widgets.Border       as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center       as C
import qualified Brick.Focus                as F
import qualified Brick.Types                as T
import qualified Brick.Util                 as U
import qualified Graphics.Vty               as V
import qualified Data.Set                   as Set


-- | The main render function for the brick app
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

-- | Rendering function for the help
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

-- | Sub-function for rendering a step
drawStep :: Int -> Step -> Maybe Equipment -> Bool -> T.Widget Name
drawStep index step focusedEquipment isFocusedStep
  | isFocusedStep =
    updateAttrMap (A.applyAttrMappings [(B.borderAttr, U.fg V.brightBlue)]) stepWidget
  | otherwise = stepWidget
  where
    stepWidget :: T.Widget Name
    stepWidget =
      withBorderStyle BS.ascii $
      B.borderWithLabel (str $ "Step " <> show (index + 1)) $
      vLimitPercent 30 $
      C.vCenter $
      vBox $
        str "Equipments: " : map (drawSelection isFocusedStep focusedEquipment) selection
    equipments :: Set Equipment
    equipments = _stepEquipments step
    selection :: [(Equipment, Bool)]
    selection = map (\e -> (e, e `Set.member` equipments)) allEquipments

-- | Sub-function for rendering a selection
drawSelection :: Bool -> Maybe Equipment -> (Equipment, Bool) -> T.Widget Name
drawSelection True (Just focusedEquipment) (e, selected) =
  drawEquipment e selected (focusedEquipment == e)
drawSelection _ _ (e, selected) = drawEquipment e selected False

-- | Sub-function for rendering a equipment
drawEquipment :: Equipment -> Bool -> Bool -> T.Widget Name
drawEquipment equipment isSelected isFocused =
  let
    icone = if isSelected then "✓" else "✕"
    attrIcone = if isSelected then "success" else "failure"
    equipmentWidget =
      let widget = str $ show equipment
      in if isFocused then withAttr "focused" widget else widget
  in
    padLeftRight 2 $
    withAttr attrIcone (str icone) <+> padLeft (T.Pad 2) equipmentWidget