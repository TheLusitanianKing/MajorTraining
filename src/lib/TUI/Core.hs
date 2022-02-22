{-# LANGUAGE OverloadedStrings #-}

module TUI.Core (app) where


import TUI.AppState (AppState, Name)
import TUI.Events (appEvent)
import TUI.Rendering (drawUI)

import qualified Brick.AttrMap as A
import qualified Brick.Main    as M
import qualified Brick.Util    as U
import qualified Graphics.Vty  as V


app :: M.App AppState e Name
app = M.App
  { M.appDraw = drawUI
  , M.appChooseCursor = M.showFirstCursor
  , M.appHandleEvent = appEvent
  , M.appStartEvent = return
  , M.appAttrMap = const attrMap
  }

attrMap :: A.AttrMap
attrMap = A.attrMap V.defAttr
  [ ("success", U.fg V.green)
  , ("failure", U.fg V.red)
  , ("key", V.white `U.on` V.blue)
  , ("importantKey", V.white `U.on` V.green)
  , ("dangerousKey", V.white `U.on` V.red)
  , ("focused", U.fg V.brightBlue)
  ]