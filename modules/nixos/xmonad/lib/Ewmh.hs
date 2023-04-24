-- | DOC:
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.0/docs/XMonad-Hooks-EwmhDesktops.html
module Ewmh where

import XMonad
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.IndependentScreens
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Hooks.UrgencyHook (doAskUrgent)


myRename :: String -> WindowSpace -> String
myRename s _w = unmarshallW s

myFilter = filterOutWs [scratchpadWorkspaceTag]

myEwmh :: XConfig a -> XConfig a
myEwmh c =
  ewmhFullscreen $
    ewmh $
      setEwmhActivateHook doAskUrgent $
        addEwmhWorkspaceSort (pure myFilter) $
          addEwmhWorkspaceRename (pure myRename) c
