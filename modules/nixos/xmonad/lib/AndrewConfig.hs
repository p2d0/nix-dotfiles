{-# LANGUAGE FlexibleInstances #-}

module AndrewConfig where

import Ewmh (myEwmh)
import Hooks.ManageHook (myManageHook)
import Hooks.Startup (myStartupHook)
import Keys (myKeys)
import Layouts (myLayout)
import Transparency (enableTransparency)
import Workspaces (myWorkspaces)
import XMonad
import XMonad.Actions.MouseGestures
import XMonad.Actions.UpdatePointer (updatePointer)
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks (docks)
import XMonad.Hooks.Minimize (minimizeEventHook)
import XMonad.Hooks.TaffybarPagerHints (pagerHints)
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageHelpers

-- instance Show (X ()) where
--   show f = "Kekw"


myConfig =
  def
    { modMask = mod4Mask,
      layoutHook = myLayout,
      terminal = "alacritty",
      workspaces = myWorkspaces,
      logHook = updatePointer (0.5, 0.5) (0, 0),
      startupHook = myStartupHook,
      handleEventHook = minimizeEventHook,
      -- <+> dynamicPropertyChange
      --   "WM_CLASS"
      --   ( composeAll
      --       [title =? "photoshop.exe" --> doFullFloat
      --       ]
      --   ),
      borderWidth = 0,
      manageHook = myManageHook
    }

mainConfig = do
  xmonad $
    docks $
      myEwmh $
          pagerHints $
            myKeys myConfig
