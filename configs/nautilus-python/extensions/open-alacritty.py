import os
import subprocess
from urllib.parse import unquote
from gi.repository import Nautilus, GObject
from typing import List

class AlacrittySpecialExtension(GObject.GObject, Nautilus.MenuProvider):
    def _open_terminal(self, file: Nautilus.FileInfo) -> None:
        uri = file.get_uri()
        if not uri.startswith("file://"):
            return
        
        target_dir = unquote(uri[7:])
        # The workspace name in Hyprland
        special_name = "alacritty" 
        # The custom class to match your hyprland.conf rule
        win_class = "alacritty-sp"

        try:
            # 1. Try to add a new 'tab' to the existing tmux session
            # We use 'tmux ls' first to see if a session actually exists
            subprocess.run(["tmux", "ls"], check=True, capture_output=True)
            subprocess.run(["tmux", "new-window", "-c", target_dir], check=True)
        
        except (subprocess.CalledProcessError, FileNotFoundError):
            # 2. If tmux isn't running, launch Alacritty with the SPECIFIC CLASS
            # and start a new tmux session inside it.
            subprocess.Popen([
                "alacritty", 
                "--class", win_class, 
                "-e", "tmux", "new-session", "-c", target_dir
            ])

        # 3. Bring your special:alacritty workspace into focus
        subprocess.run(["hyprctl", "dispatch", "togglespecialworkspace", special_name])

    def menu_activate_cb(self, menu, file):
        self._open_terminal(file)

    def menu_background_activate_cb(self, menu, file):
        self._open_terminal(file)

    def get_file_items(self, files: List[Nautilus.FileInfo]) -> List[Nautilus.MenuItem]:
        if len(files) != 1 or not files[0].is_directory():
            return []
        item = Nautilus.MenuItem(
            name="NautilusPython::alacritty_special_file",
            label="Open in Special Alacritty",
            tip="Open in special:alacritty using tmux",
        )
        item.connect("activate", self.menu_activate_cb, files[0])
        return [item]

    def get_background_items(self, current_folder: Nautilus.FileInfo) -> List[Nautilus.MenuItem]:
        item = Nautilus.MenuItem(
            name="NautilusPython::alacritty_special_bg",
            label="Open in Special Alacritty",
            tip="Open in special:alacritty using tmux",
        )
        item.connect("activate", self.menu_background_activate_cb, current_folder)
        return [item]
