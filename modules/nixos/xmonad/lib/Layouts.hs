--
module Layouts where

import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Layout.BoringWindows
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.LimitWindows (limitWindows)
import XMonad.Layout.Magnifier (magnifiercz')
import XMonad.Layout.Minimize
import XMonad.Layout.MultiColumns
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect (reflectHoriz)
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.TwoPane
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SimplestFloat (simplestFloat)

myTabConfig =
  def
    { activeColor = "#556064",
      inactiveColor = "#2F3D44",
      urgentColor = "#FDF6E3",
      activeBorderColor = "#454948",
      inactiveBorderColor = "#454948",
      urgentBorderColor = "#268BD2",
      activeTextColor = "#80FFF9",
      inactiveTextColor = "#1ABC9C",
      urgentTextColor = "#1ABC9C",
      fontName = "xft:Noto Sans CJK:size=10:antialias=true"
    }

-- Choose
--   (ModifiedLayout LimitWindows (ModifiedLayout Magnifier Tall))
--   (Choose
--      (ModifiedLayout
--         (Decoration TabbedDecoration DefaultShrinker) Simplest)
--      (ModifiedLayout LimitWindows (ModifiedLayout Magnifier ThreeCol)))
-- Window -> b
-- host specific
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Layout-OnHost.html

-- modifiers :: l -> ModifiedLayout m l a
modifiers = lessBorders Screen . avoidStrutsOn [D] . (minimize . boringAuto) -- TODO generic type signature

modifiers2 = lessBorders Screen . avoidStrutsOn [D] . (minimize . boringAuto) -- ??????????????????

modifiers3 = lessBorders Screen . avoidStrutsOn [D] . (minimize . boringAuto) -- ??????????????????


myLayout =
  onWorkspace "1_10" (stackTile ||| Full) $
  onWorkspace "1_6" (simplestFloat ||| Full) $
    onWorkspaces ["0_" ++ show b | b <- [1 .. 5]] (modifiers2 (reflectHoriz tiled ||| threeCol ||| tabbedBottom shrinkText myTabConfig)) $
      modifiers (tiled ||| tabbedBottom shrinkText myTabConfig ||| simplestFloat)
        ||| Full
  where
    stackTile = modifiers3 $ reflectHoriz (Tall 1 0.03 0.5)

    columns = multiCol [1] 1 0.01 0.5

    tiled = limitWindows 3 $ Tall nmaster delta ratio
    threeCol = ThreeColMid nmaster delta 0.5

    nmaster = 1
    ratio = 0.7
    delta = 3 / 100

-- drawer = Layout.drawer 0.0 0.4 (ClassName "Spotify" `Or` ClassName "Telegram" `Or` ClassName "Org.gnome.Nautilus") Full
