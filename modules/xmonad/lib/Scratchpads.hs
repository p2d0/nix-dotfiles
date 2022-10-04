-- |

module Scratchpads where
import XMonad.Util.NamedScratchpad
import XMonad
import XMonad.Hooks.ManageHelpers
import qualified XMonad.StackSet as W

myScratchpads =
  [ -- NS "calc" "gnome-calculator" (className =? "Gnome-calculator") defaultFloating
    NS "calc" "speedcrunch" (className =? "SpeedCrunch") (doRectFloat (W.RationalRect 0.8 0.20 0.2 0.6)),
    NS "chat" "chatterino" (className =? "chatterino") (doRectFloat (W.RationalRect 0.85 0.20 0.15 0.6))
  ]
