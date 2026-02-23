#!/usr/bin/env python3

import json
import subprocess
import re

def get_hypr_data(command):
    """Helper to run hyprctl commands and return JSON."""
    try:
        result = subprocess.run(['hyprctl', command, '-j'], capture_output=True, text=True)
        return json.loads(result.stdout)
    except Exception as e:
        print(f"Error fetching Hyprland data: {e}")
        return None

def find_window(title_regex=None, class_regex=None, workspace_name=None):
    """
    Finds a window's hex address based on title, class, and/or workspace.
    Returns something like 'address:0x55ca12345670' or None.
    """
    clients = get_hypr_data('clients')
    if not clients:
        return None

    for client in clients:
        match = True
        
        if title_regex and not re.search(title_regex, client.get('title', ''), re.IGNORECASE):
            match = False
        if class_regex and not re.search(class_regex, client.get('class', ''), re.IGNORECASE):
            match = False
        if workspace_name and client.get('workspace', {}).get('name', '') != workspace_name:
            match = False
            
        if match:
            # Important: Hyprland expects the '0x' prefix in dispatchers
            addr = client['address']
            if not addr.startswith('0x'):
                addr = '0x' + addr
            return f"address:{addr}"
    return None

def resize_window(address, w, h):
    """Resizes a window to exact pixel dimensions."""
    if not address:
        return
    # Ensure window is floating first
    subprocess.run(['hyprctl', 'dispatch', 'setfloating', address], capture_output=True)
    command = ['hyprctl', 'dispatch', 'resizewindowpixel', f'exact {w} {h},{address}']
    print(f"Resizing {address} to {w}x{h}")
    subprocess.run(command)

def move_window(address, x, y):
    """Moves a window to exact coordinates using its address."""
    if not address:
        return
    # Ensure window is floating first
    subprocess.run(['hyprctl', 'dispatch', 'setfloating', address], capture_output=True)
    command = ['hyprctl', 'dispatch', 'movewindowpixel', f'exact {x} {y},{address}']
    print(f"Moving {address} to {x}, {y}")
    subprocess.run(command)

def focus_window(address):
    """Focuses a window by its address."""
    if address:
        subprocess.run(['hyprctl', 'dispatch', 'focuswindow', address])

def main():
    monitors = get_hypr_data('monitors')
    if not monitors:
        return

    # Find active monitor
    active_monitor = next((m for m in monitors if m.get('focused')), None)
    if not active_monitor:
        print("No active monitor found.")
        return

    # 1. Identify Windows
    # Specifically find emacs-todo on the special workspace
    emacs_addr = find_window(title_regex="emacs-todo", workspace_name="special:emacs")
    # Find Firefox (searching by class "firefox" is usually safer than title)
    browser_addr = find_window(class_regex="firefox", workspace_name="special:emacs")

    # 2. Apply Resizing to Firefox
    if browser_addr:
        resize_window(browser_addr, 400, 900)
    else:
        print("Could not find Firefox window to resize.")

    # 3. Position the windows based on monitor
    # Standard dimensions for your math
    window_width = 800
    calendar_gap = 10
    window_width_calendar = 400
    window_height = 900
    
    if  active_monitor["description"] != "LG Electronics LG HDR WFHD 0x00077717":
        width = active_monitor['width']
        height = active_monitor['height']
        
        # Position Browser
        position_x = 2560 + (width // 2  - window_width)
        position_y = (height - window_height) // 2
        move_window(browser_addr, position_x, position_y)

        # Position Emacs
        position_x_calendar = position_x + window_width_calendar + calendar_gap + 100
        move_window(emacs_addr, position_x_calendar, position_y)
    else:
        width = active_monitor['width']
        height = active_monitor['height']

        # Position Emacs
        position_x = width // 2 - 250
        position_y = (height - window_height) // 2
        move_window(emacs_addr, position_x, position_y)

        # Position Browser
        position_x_calendar = position_x - window_width_calendar - calendar_gap - 100
        move_window(browser_addr, position_x_calendar, position_y)

    # 4. Handle Focus logic
    # Only focus if the specific special workspace is currently visible
    special_workspace_info = active_monitor.get('specialWorkspace', {})
    if special_workspace_info.get('name') == 'special:emacs':
        if emacs_addr:
            focus_window(emacs_addr)

if __name__ == "__main__":
    main()
