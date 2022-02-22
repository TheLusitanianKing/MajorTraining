{-# LANGUAGE OverloadedStrings #-}

module TUI where


import Data.Set (Set)
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

import qualified Data.Set                   as Set


data AppState = AppState
  { _apsCircuit          :: Circuit
  , _apsFocusedStep      :: F.FocusRing (Int, Step) -- TODO: to be continued..
  , _apsFocusedEquipment :: F.FocusRing Equipment
  , _apsInterrupted      :: Bool
  }

instance Show AppState where
  show st = "AppState Circuit: " <> show (_apsCircuit st)

initialAppState :: AppState
initialAppState = AppState
  { _apsCircuit = circuit
  , _apsFocusedStep = F.focusRing . zip [1..] . _circuitSteps $ circuit
  , _apsFocusedEquipment = F.focusRing allEquipments
  , _apsInterrupted = False -- if true, does not need to generate the circuit
  }
  where
    circuit = Circuit [Step $ Set.fromList [Wall], Step Set.empty]

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
    steps = map (drawStep focusedEquipment) . _circuitSteps . _apsCircuit $ as
    ui =
      hBox steps <=> helperCommands

-- TODO: simplify that
helperCommands :: T.Widget Name
helperCommands =
  vBox
    [ withAttr "key" (str "Up-down") <+> str ": Navigate between the equipments of the selected step"
    , withAttr "key" (str "Enter") <+> str ": Select/deselect the focused equipment"
    , withAttr "key" (str "Left-Right") <+> str ": Navigate between the steps"
    , withAttr "key" (str "+") <+> str ": Add a step"
    , withAttr "key" (str "-") <+> str ": Remove the currently focused step"
    , withAttr "importantKey" (str "Space") <+> str ": Start the generation"
    , withAttr "dangerousKey" (str "Esc") <+> str ": Quit"
    ]

drawStep :: Maybe Equipment -> Step -> T.Widget Name
drawStep mequipment step =
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
    map (drawSelection mequipment) selection

drawSelection :: Maybe Equipment -> (Equipment, Bool) -> T.Widget Name
drawSelection (Just eq) (e, selected) = drawEquipment selected (eq == e) e
drawSelection _ (e, selected) = drawEquipment selected False e

drawEquipment :: Bool -> Bool -> Equipment -> T.Widget Name
drawEquipment isSelected isFocused equipment =
  let
    icone = if isSelected then "✓" else "✕"
    attrIcone  = if isSelected then "success" else "failure"
    equipmentWidget =
      if isFocused 
        then withAttr "focused" . str $ show equipment
        else str $ show equipment
  in
  padLeftRight 2 $
  withAttr attrIcone (str icone) <+> padLeft (T.Pad 2) equipmentWidget

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt $ st { _apsInterrupted = True }
    V.EvKey (V.KChar ' ') [] -> M.halt st
    V.EvKey V.KUp [] ->
      -- TODO: improve this with lenses
      M.continue $
        st { _apsFocusedEquipment = F.focusPrev (_apsFocusedEquipment st) }
    V.EvKey V.KDown [] ->
      M.continue $
        st { _apsFocusedEquipment = F.focusNext (_apsFocusedEquipment st) }
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