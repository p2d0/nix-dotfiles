-- |
module Hooks.Startup where

import Data.Foldable
import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.SpawnOnce
import XMonad.Actions.SpawnOn

myStartupHook =
  spawnAllOnce
    [ "pasystray",
      -- "/usr/lib/kdeconnectd",
      -- "~/.dropbox-dist/dropboxd",
      "dunst",
      "my-taffybar",
      "flameshot",
      -- "emacs -nw --daemon",
      -- "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1",
      "blueman-applet",
      -- "picom --experimental-backends",
      -- "xcompmgr",
      "guake",
      -- "fcitx5",
      "redshift-gtk"
    ]
    -- <+> spawnAndDoOnce (doShift "1_10") "telegram-desktop"
    -- <+> spawnAndDoOnce (doShift "1_10") "sleep 5;/mnt/md126/Downloads/OpenHab-linux-x64/OpenHab"

spawnAllOnce xs =
  forM_ xs spawnOnce
