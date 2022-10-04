module AndrewConfigTest where
import Test.HUnit
import XMonad (screenWorkspace)
import Control.Monad.Trans.State.Lazy (get)
import XMonad.Core (runX)

tests = TestList [
  TestCase $ do
      (Just workspaceId) <- runX $ screenWorkspace 0
      assertEqual ""  workspaceId 1
  ]
