#!/usr/bin/env sh

# Ensure the path to hyprlock is correct
# You can find the path by running 'which hyprlock'
HYPRLOCK="/run/current-system/sw/bin/hyprlock"

# Export the required session variables
export XDG_RUNTIME_DIR="/run/user/1000"
export WAYLAND_DISPLAY="wayland-0"
export DBUS_SESSION_BUS_ADDRESS="unix:path=/run/user/1000/bus"

$HYPRLOCK


# Check if the file /tmp/emergency does not exist
# if [ ! -e /tmp/emergency ]; then
#     # If the file does not exist, shut down the system
#     # shutdown -h now
#     # pkill Hyprland
#     # pkill i3
# else
#     # If the file exists, do nothing
#     echo "File /tmp/emergency exists. No action taken."
#     rm /tmp/emergency
# fi
