module TUI where


import Model

import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import qualified Brick.Main as M
import qualified Brick.Types as T

data AppState = AppState
  { _apsCircuit   :: Circuit
  , _apsFocusRing :: F.FocusRing Name
  }

data Name
  = SelectedStep (Int, Int) -- (step index, equipment index)
  | AddStep
  | RemoveStep
  | Validate
  deriving (Show)

app :: M.App AppState e Name
app = M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attrMap
  }

drawUI :: AppState -> [T.Widget Name]
drawUI = map drawStep . _circuitSteps . _apsCircuit

drawStep :: Step -> T.Widget Name
drawStep = undefined

appEvent :: AppState -> T.BrickEvent Name e -> T.EventM Name (T.Next AppState)
appEvent = undefined

attrMap :: A.AttrMap
attrMap = undefined