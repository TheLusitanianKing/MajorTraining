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
  { _apsCircuit   :: Circuit
  , _apsFocusRing :: F.FocusRing Name
  }

instance Show AppState where
  show st = "AppState Circuit: " <> show (_apsCircuit st)

initialAppState :: AppState
initialAppState = AppState
  { _apsCircuit = Circuit [Step Set.empty, Step Set.empty]
  , _apsFocusRing = undefined -- we don't even care for now
  }

data Name
  = SelectedStep (Int, Int) -- (step index, equipment index)
  | AddStep
  | RemoveStep
  | Validate
  deriving (Eq, Ord, Show)

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
    steps = map drawStep . _circuitSteps . _apsCircuit $ as
    ui = hBox steps <=> str "THIS IS A DESCRIPTION"

drawStep :: Step -> T.Widget Name
drawStep step =
  let
    equipments :: Set Equipment
    equipments = _stepEquipments step
    allEquipments :: [Equipment]
    allEquipments = [minBound..maxBound]
    selection :: [(Equipment, Bool)]
    selection = map (\e -> (e, e `Set.member` equipments)) allEquipments
  in
  withBorderStyle BS.ascii $
  B.borderWithLabel (str "Step") $
  vLimitPercent 30 $
  C.vCenter $
  vBox $
    map drawSelection selection

drawSelection :: (Equipment, Bool) -> T.Widget Name
drawSelection (e, False) =
  padLeftRight 2 $
  withAttr "failure" (str "✕") <+> padLeft (T.Pad 2) (str $ show e)
drawSelection (e, True) =
  padLeftRight 2 $
  withAttr "success" (str "✓") <+> padLeft (T.Pad 2) (str $ show e)

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent st (T.VtyEvent ev) =
  case ev of
    V.EvKey V.KEsc [] -> M.halt st
    _ -> M.continue st
appEvent st _ = M.continue st

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
  [ ("success", U.fg V.green)
  , ("failure", U.fg V.red)
  ]