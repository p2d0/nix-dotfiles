--
module Hooks.ManageHook where

import Data.List
import qualified Debug.Trace
import Scratchpads
import XMonad
import XMonad.Config.Dmwit (floatAll)
import XMonad.Hooks.DynamicProperty
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Place (inBounds, placeHook, simpleSmart, underMouse, withGaps)
import XMonad.Layout.NoBorders (hasBorder)
import qualified XMonad.StackSet as W
import XMonad.Util.NamedScratchpad

contains :: (Eq a, Functor m, Show a) => m [a] -> [a] -> m Bool
q `contains` x = fmap (\s -> Debug.Trace.trace (show x ++ " isInfixOf " ++ show s ++ " " ++ show (x `isInfixOf` s)) (x `isInfixOf` s)) q

-- containz :: Query String -> String -> Query Bool
-- q `containz` x = fmap (\s -> x `isInfixOf` s) q
-- https://hackage.haskell.org/package/xmonad-contrib-0.17.1/docs/XMonad-Hooks-Place.html TODO fix
myPlacement = withGaps (16, 0, 16, 0) (underMouse (0, 0))

myManageHook =
  namedScratchpadManageHook myScratchpads
    <+> placeHook myPlacement
    <+> composeAll
      [ -- https://stackoverflow.com/questions/26028146/xmonad-open-a-window-into-a-particular-tile
        className =? "TelegramDesktop" --> doShift "1_10",
        className =? "TelegramDesktop" --> hasBorder False,
        className =? "tabbed" --> hasBorder False,
        className =? "discord" --> doShift "1_8",
        className =? "qBittorrent" --> doShift "1_8",
        className =? ".guake-wrapped" --> hasBorder False,
        className =? "YouTube Music" --> doShift "1_10" <+> doF W.swapUp,
        className =? "YouTube Music" --> doShift "1_10" <+> doF W.swapUp,
        className =? "Rocket.Chat" --> doShift "1_10" <+> doF W.swapUp,
        className =? "openhab-nativefier-9825c2" --> doShift "1_10"
      ]
    <+> composeOne -- https://bbs.archlinux.org/viewtopic.php?id=98695
      [ title =? "Media viewer" -?> doIgnore,
        className =? "Steam" -?> doFloat,
        className =? "steam" -?> doFullFloat,
        className =? ".guake-wrapped" -?> doFloat,
        className =? "Guake" -?> doFloat,
        className =? "Pavucontrol" -?> doFloat,
        -- The rectangle to float the window in. 0 to 1; x, y, w, h.
        -- className =? "Org.gnome.Nautilus" -?> doRectFloat (W.RationalRect 0.7 0.6 0.3 0.4),
        className =? "Gnome-boxes" -?> doFloat,
        className =? "gnome-boxes" -?> doFloat,
        className =? "Boxes" -?> doFloat,
        title =? "emacs-everywhere" -?> doFloat,
        title =? "org-roam-everywhere" -?> doFloat,
        title =? "emacs-note" -?> doFloat,
        className =? "Blueman-manager" -?> doCenterFloat,
        className =? "blueman-manager-wrapped" -?> doFloat,
        isDialog -?> doFloat,
        isFullscreen -?> doFullFloat,
        return True -?> insertPosition Below Older
      ]
