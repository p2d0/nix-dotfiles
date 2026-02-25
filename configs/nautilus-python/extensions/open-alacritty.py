import os
from urllib.parse import unquote
from gi.repository import Nautilus, GObject
from typing import List
import subprocess

class OpenAlacrittyTmuxExtension(GObject.GObject, Nautilus.MenuProvider):
    def _open_terminal(self, file: Nautilus.FileInfo) -> None:
        uri = file.get_uri()
        if not uri.startswith("file://"):
            return
        
        target_dir = unquote(uri[7:])

        try:
            # 1. Check if a tmux session exists
            # We try to create a new window in the currently attached session.
            # If no session is attached/running, this will fail.
            subprocess.run(
                ["tmux", "new-window", "-c", target_dir],
                check=True,
                capture_output=True
            )
            
            # (Optional) Use a tool like wmctrl to focus the Alacritty window 
            # if it's buried under other windows.
            # subprocess.run(["wmctrl", "-a", "Alacritty"], check=False)

        except (subprocess.CalledProcessError, FileNotFoundError):
            # 2. Fallback: If tmux is not running or not installed, 
            # start Alacritty with a fresh tmux session.
            try:
                # -e runs the command. We start tmux and set the starting dir.
                subprocess.Popen([
                    "alacritty", 
                    "-e", "tmux", "new-session", "-c", target_dir
                ])
            except FileNotFoundError:
                print("Error: Alacritty or tmux not found.")

    def menu_activate_cb(self, menu, file):
        self._open_terminal(file)

    def menu_background_activate_cb(self, menu, file):
        self._open_terminal(file)

    def get_file_items(self, files: List[Nautilus.FileInfo]) -> List[Nautilus.MenuItem]:
        if len(files) != 1 or not files[0].is_directory():
            return []
        item = Nautilus.MenuItem(
            name="NautilusPython::openalacritty_tmux_file",
            label="Open in Alacritty (Tmux)",
            tip=f"Open Tmux window in {files[0].get_name()}",
        )
        item.connect("activate", self.menu_activate_cb, files[0])
        return [item]

    def get_background_items(self, current_folder: Nautilus.FileInfo) -> List[Nautilus.MenuItem]:
        item = Nautilus.MenuItem(
            name="NautilusPython::openalacritty_tmux_bg",
            label="Open in Alacritty (Tmux)",
            tip=f"Open Tmux window in {current_folder.get_name()}",
        )
        item.connect("activate", self.menu_background_activate_cb, current_folder)
        return [item]
