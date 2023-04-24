{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module ClickableWidget where

import Control.Monad
import Control.Monad.IO.Class
import Data.Monoid
import Data.Text
import qualified GI.Gdk as Gdk
import qualified GI.Gtk as Gtk
import GI.Gtk.Objects.Widget
import System.Taffybar.Context
import System.Taffybar.Widget (commandRunnerNew)
import System.Taffybar.Widget.Generic.PollingLabel
import SimpleCmd (shell)
import qualified Data.Text as T

data ClickableWidgetConfig = ClickableWidgetConfig
  { pollingPeriod :: Double,
    command :: String,
    commandArguments :: [String],
    onLeftClick :: String,
    onMiddleClick :: String,
    onRightClick :: String,
    onFail :: Text
  }

caffCallback conf event =
  liftIO $ do
    button <- Gdk.getEventButtonButton event
    case button of
      1 -> shell $ onLeftClick conf
      2 -> shell $ onMiddleClick conf
      3 -> shell $ onRightClick conf
    return False

wrap ::
  MonadIO m =>
  Gtk.Widget ->
  ClickableWidgetConfig ->
  m Gtk.Widget
wrap button conf = liftIO $ do
  ebox <- Gtk.eventBoxNew
  Gtk.containerAdd ebox button
  Gtk.eventBoxSetVisibleWindow ebox False
  _ <- Gtk.onWidgetButtonPressEvent ebox (caffCallback conf)
  Gtk.widgetShowAll ebox
  Gtk.toWidget ebox

clickableWidget :: MonadIO m => ClickableWidgetConfig -> m Gtk.Widget
clickableWidget conf =
  liftIO $ do
  btn <- pollingLabelNew
        (pollingPeriod conf)
        (T.pack <$> shell (command conf))
  wrap btn conf
