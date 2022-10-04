-- |
module Transparency where

import XMonad
import XMonad.Hooks.FadeWindows (fadeWindowsEventHook, fadeWindowsLogHook, transparency, isFloating)

myFadeHook =
  composeAll
    [ className =? "chatterino" --> transparency 0.2,
      className =? "SpeedCrunch" --> transparency 0.2
      -- isFloating --> transparency 0.1
    ]

enableTransparency :: XConfig a -> XConfig a
enableTransparency c =
  c
    { handleEventHook = fadeWindowsEventHook <+> handleEventHook c,
      logHook = fadeWindowsLogHook myFadeHook <+> logHook c
    }
