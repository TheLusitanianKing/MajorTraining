module TUI where


import Model

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T


app :: M.App Circuit e ()
app = M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attrMap
  }

drawUI :: Circuit -> [T.Widget ()]
drawUI = undefined

appEvent :: Circuit -> T.BrickEvent () e -> T.EventM () (T.Next Circuit)
appEvent = undefined

attrMap :: A.AttrMap
attrMap = undefined