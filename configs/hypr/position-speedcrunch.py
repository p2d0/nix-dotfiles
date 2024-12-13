#!/usr/bin/env python3

import json
import subprocess

def get_active_monitor():
    # Run hyprctl monitors and parse the JSON output
    result = subprocess.run(['hyprctl', 'monitors', '-j'], capture_output=True, text=True)
    monitors = json.loads(result.stdout)

    # Find the active monitor
    for monitor in monitors:
        if monitor.get('focused', False):
            return monitor
    return None

def move_window(offset_x):
    # Move the window by the calculated offset
    subprocess.run(['hyprctl', 'dispatch', 'movewindowpixel', str(offset_x), '0', ',class:speedcrunch'])

def main():
    active_monitor = get_active_monitor()
    if active_monitor:
        width = active_monitor['width']

        # Calculate the offset to move the window to the right edge
        window_width = 310
        offset_x = (width // 2) - (window_width // 2)  # Offset from the center to the right edge

        move_window(offset_x)
    else:
        print("No active monitor found.")

if __name__ == "__main__":
    main()
