#!/usr/bin/env bash

# Terminate already running bar instances
# killall -q waybar
pkill waybar

# Wait until the processes have been shut down
while pgrep waybar > /dev/null; do sleep 1; done

# Launch bars
waybar
