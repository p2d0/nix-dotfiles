{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      : System.Taffybar.Widget.Windows
-- Copyright   : (c) Ivan Malison
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Ivan Malison <IvanMalison@gmail.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Menu widget that shows the title of the currently focused window and that,
-- when clicked, displays a menu from which the user may select a window to
-- which to switch the focus.
-----------------------------------------------------------------------------

module System.Taffybar.Widget.Windows where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Default (Default(..))
import           Data.Maybe
import qualified Data.Text as T
import           GI.GLib (markupEscapeText)
import qualified GI.Gtk as Gtk
import           System.Taffybar.Context
import           System.Taffybar.Information.EWMHDesktopInfo
import           System.Taffybar.Util
import           System.Taffybar.Widget.Generic.DynamicMenu
import           System.Taffybar.Widget.Util

data WindowsConfig = WindowsConfig
  { getMenuLabel :: X11Window -> TaffyIO T.Text
  -- ^ A monadic function that will be used to make a label for the window in
  -- the window menu.
  , getActiveLabel :: TaffyIO T.Text
  -- ^ Action to build the label text for the active window.
  }

defaultGetMenuLabel :: X11Window -> TaffyIO T.Text
defaultGetMenuLabel window = do
  windowString <- runX11Def "(nameless window)" (getWindowTitle window)
  return $ T.pack windowString

defaultGetActiveLabel :: TaffyIO T.Text
defaultGetActiveLabel = do
  label <- fromMaybe "" <$> (runX11Def Nothing getActiveWindow >>=
                                       traverse defaultGetMenuLabel)
  markupEscapeText label (-1)

truncatedGetActiveLabel :: Int -> TaffyIO T.Text
truncatedGetActiveLabel maxLength =
  truncateText maxLength <$> defaultGetActiveLabel

truncatedGetMenuLabel :: Int -> X11Window -> TaffyIO T.Text
truncatedGetMenuLabel maxLength =
  fmap (truncateText maxLength) . defaultGetMenuLabel

defaultWindowsConfig :: WindowsConfig
defaultWindowsConfig =
  WindowsConfig
  { getMenuLabel = truncatedGetMenuLabel 35
  , getActiveLabel = truncatedGetActiveLabel 35
  }

instance Default WindowsConfig where
  def = defaultWindowsConfig

-- | Create a new Windows widget that will use the given Pager as
-- its source of events.
windowsNew :: WindowsConfig -> TaffyIO Gtk.Widget
windowsNew config = do
  label <- lift $ Gtk.labelNew Nothing

  let setLabelTitle title = lift $ postGUIASync $ Gtk.labelSetMarkup label title
      activeWindowUpdatedCallback _ = getActiveLabel config >>= setLabelTitle

  subscription <-
    subscribeToPropertyEvents [ewmhActiveWindow, ewmhWMName, ewmhWMClass]
                      activeWindowUpdatedCallback
  _ <- liftReader (\x -> Gtk.onWidgetUnrealize label x) (unsubscribe subscription)

  context <- ask

  labelWidget <- Gtk.toWidget label
  menu <- dynamicMenuNew
    DynamicMenuConfig { dmClickWidget = labelWidget
                      , dmPopulateMenu = flip runReaderT context . fillMenu config
                      }

  widgetSetClassGI menu "windows"

-- | Populate the given menu widget with the list of all currently open windows.
fillMenu :: Gtk.IsMenuShell a => WindowsConfig -> a -> ReaderT Context IO ()
fillMenu config menu = ask >>= \context ->
  runX11Def () $ do
    windowIds <- getWindows
    forM_ windowIds $ \windowId ->
      lift $ do
        labelText <- runReaderT (getMenuLabel config windowId) context
        let focusCallback = runReaderT (runX11 $ focusWindow windowId) context >>
                            return True
        item <- Gtk.menuItemNewWithLabel labelText
        _ <- Gtk.onWidgetButtonPressEvent item $ const focusCallback
        Gtk.menuShellAppend menu item
        Gtk.widgetShow item
