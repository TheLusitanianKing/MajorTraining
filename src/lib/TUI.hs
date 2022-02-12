module TUI where


import Model

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T


app :: M.App AppState e ()
app = M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attrMap
  }

drawUI :: AppState -> [T.Widget ()]
drawUI = map drawStep . _circuitSteps . _apCircuit

drawStep :: Step -> T.Widget ()
drawStep = undefined

appEvent :: AppState -> T.BrickEvent () e -> T.EventM () (T.Next AppState)
appEvent = undefined

attrMap :: A.AttrMap
attrMap = undefined