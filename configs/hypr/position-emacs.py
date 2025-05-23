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
    subprocess.run(['hyprctl', 'dispatch', 'movewindowpixel exact', f'{offset_x} {offset_y},', selector])

def main():
    active_monitor = get_active_monitor()
    gap_x = 30
    window_width = 800
    calendar_gap = 10
    window_width_calendar = 600
    window_height = 910
    if(active_monitor['name'] == 'HDMI-A-1'):
        width = active_monitor['width']
        height = active_monitor['height']

        position_x = 2560 + (width // 2  - window_width)
        position_y = (height - window_height) // 2

        # Move the window
        move_window(position_x, position_y, 'class:brave-calendar.notion.so*+')

        position_x_calendar = position_x + window_width_calendar + calendar_gap
        move_window(position_x_calendar, position_y, 'title:emacs-todo')
    else:
        width = active_monitor['width']
        height = active_monitor['height']


        position_x = width // 2
        position_y = (height - window_height) // 2

        # Move the window
        move_window(position_x, position_y, 'title:emacs-todo')

        position_x_calendar = position_x - window_width_calendar - calendar_gap
        move_window(position_x_calendar, position_y, 'class:brave-calendar.notion.so*+')

    # if active_monitor:
    #     second_monitor_offset = 0;
    #     if(active_monitor['name'] == 'HDMI-A-2'):
    #         second_monitor_offset = 2560

    #     width = active_monitor['width']
    #     height = active_monitor['height']

    #     # Define window dimensions
    #     window_width = 610  # Updated from 610 to 810 as per your code
    #     window_height = 810  # Assuming a reasonable height; adjust as needed

    #     position_x = width - window_width + second_monitor_offset
    #     position_y = (height - window_height) // 2

    #     # Move the window
    #     move_window(position_x, position_y, 'title:emacs-todo')

    #     position_x_calendar = position_x - window_width
    #     move_window(position_x_calendar, position_y, 'class:brave-calendar.notion.so*+')
    # else:
    #     print("No active monitor found.")

if __name__ == "__main__":
    main()
