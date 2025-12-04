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

def move_window(offset_x, offset_y, selector='title:emacs-todo'):
    # Move the window by the calculated offsets
    # Syntax: movewindowpixel exact x y,address
    subprocess.run(['hyprctl', 'dispatch', 'movewindowpixel', f'exact {offset_x} {offset_y},{selector}'])

def focus_window(selector='title:emacs-todo'):
    # Focus the specific window
    subprocess.run(['hyprctl', 'dispatch', 'focuswindow', selector])

def main():
    active_monitor = get_active_monitor()
    
    if not active_monitor:
        print("No active monitor found.")
        return

    # Defaults
    window_width = 800
    calendar_gap = 10
    window_width_calendar = 600
    window_height = 910
    
    # 1. Position the windows based on monitor
    if active_monitor['name'] == 'HDMI-A-1':
        width = active_monitor['width']
        height = active_monitor['height']

        position_x = 2560 + (width // 2  - window_width)
        position_y = (height - window_height) // 2

        # Move Brave
        move_window(position_x, position_y, 'class:brave-calendar.notion.so.+')

        position_x_calendar = position_x + window_width_calendar + calendar_gap
        # Move Emacs
        move_window(position_x_calendar, position_y, 'title:emacs-todo')
    else:
        width = active_monitor['width']
        height = active_monitor['height']

        position_x = width // 2 - 250
        position_y = (height - window_height) // 2

        # Move Emacs
        move_window(position_x, position_y, 'title:emacs-todo')

        position_x_calendar = position_x - window_width_calendar - calendar_gap
        # Move Brave
        move_window(position_x_calendar, position_y, 'class:brave-calendar.notion.so.+')

    # 2. Check if the special workspace is active
    # hyprctl monitors returns a dictionary object for specialWorkspace
    # e.g., { "id": 0, "name": "" } if inactive
    # e.g., { "id": -98, "name": "special:emacs" } if active
    special_workspace_info = active_monitor.get('specialWorkspace', {})
    active_special_name = special_workspace_info.get('name', '')

    # 3. Only focus if special:emacs is NOT active
    if active_special_name == 'special:emacs':
        focus_window('title:emacs-todo')

if __name__ == "__main__":
    main()
