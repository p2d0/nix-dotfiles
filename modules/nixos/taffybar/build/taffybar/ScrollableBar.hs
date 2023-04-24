module ScrollableBar where

import qualified Control.Concurrent.MVar as MV
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified GI.Gtk as Gtk
import System.Taffybar.Context (BarConfig, TaffyIO, existingWindows, logIO)

callback event =
  liftIO $ do
    putStrLn "Poggers"
    return False

wrap ::
  MV.MVar [(BarConfig, Gtk.Window)] ->
  TaffyIO ()
wrap windows = liftIO $ do
  ebox <- Gtk.eventBoxNew
  MV.modifyMVar_
    windows
    ( \w ->
        liftIO $ do
          putStrLn "Pog"
          _ <- Gtk.onWidgetButtonPressEvent (snd (head w)) callback
          return w
    )

-- [(config, window)] <- windows
-- widget <- Gtk.windowGetDefaultWidget window
-- case widget of
--   Nothing -> do
--     putStrLn "No widget"
--     windows
--   Just w -> do
--     _ <- Gtk.onWidgetButtonPressEvent w callback
--     putStrLn "Widget"
--     MV.modifyMVar
--       windows
--       ( \w ->
--           pure w
--       )
--     windows

addScrolling :: TaffyIO()
addScrolling =
  do
    windows <- asks existingWindows
    wrap windows
