# This example is contributed by Martin Enlund
import os
from urllib.parse import unquote
from gi.repository import Nautilus, GObject
from typing import List
import subprocess

class OpenTerminalExtension(GObject.GObject, Nautilus.MenuProvider):
    def _open_terminal(self, file: Nautilus.FileInfo) -> None:
        filename = unquote(file.get_uri()[7:])

        os.chdir(filename)
        target_dir = os.getcwd();
        print(os.getcwd());

        # Get the basename for the tab name
        tab_name = os.path.basename(target_dir)
        try:
            # Run guake with the specified directory and tab name
            subprocess.run(
                ["guake", "--show", "-n", target_dir, "-r", tab_name, "-t"],
                check=True
            )
        except FileNotFoundError:
            print("Error: Guake is not installed or not found")
        except subprocess.CalledProcessError:
            print("Error: Failed to open Guake")
        except OSError as e:
            print(f"Error: {e}")

    def menu_activate_cb(
        self,
        menu: Nautilus.MenuItem,
        file: Nautilus.FileInfo,
    ) -> None:
        self._open_terminal(file)

    def menu_background_activate_cb(
        self,
        menu: Nautilus.MenuItem,
        file: Nautilus.FileInfo,
    ) -> None:
        self._open_terminal(file)

    def get_file_items(
        self,
        files: List[Nautilus.FileInfo],
    ) -> List[Nautilus.MenuItem]:
        if len(files) != 1:
            return []

        file = files[0]
        if not file.is_directory() or file.get_uri_scheme() != "file":
            return []

        item = Nautilus.MenuItem(
            name="NautilusPython::openterminal_file_item",
            label="Open Guake",
            tip="Open Guake In %s" % file.get_name(),
        )
        item.connect("activate", self.menu_activate_cb, file)

        return [
            item,
        ]

    def get_background_items(
        self,
        current_folder: Nautilus.FileInfo,
    ) -> List[Nautilus.MenuItem]:
        item = Nautilus.MenuItem(
            name="NautilusPython::openterminal_file_item2",
            label="Open Guake",
            tip="Open Terminal In %s" % current_folder.get_name(),
        )
        item.connect("activate", self.menu_background_activate_cb, current_folder)

        return [
            item,
        ]
