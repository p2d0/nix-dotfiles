-- |

module Workspaces where
import XMonad.Layout.IndependentScreens
import Data.List (nub)

myWorkspaces :: [PhysicalWorkspace] -- < i.e 1_1 1_2 1_3
myWorkspaces =
  withScreen 0 ["1", "2", "3", "4", "5"]
    ++ withScreen 1 ["6", "7", "8", "9", "10"]

myWorkspaces' :: [VirtualWorkspace] -- < i.e 1 2 3 4 5
myWorkspaces' = nub $ map unmarshallW myWorkspaces
