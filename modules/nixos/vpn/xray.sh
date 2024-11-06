#!/usr/bin/env bash

# Function to run Xray
function run_xray {
    xray run -c /home/andrew/Dropbox/xray/config.json &
}

# Function to check if Xray is running
function check_xray_running {
    if pgrep xray > /dev/null; then
        return 0  # Xray is running
    else
        return 1
    fi
}

# Function to restart Xray
function restart_xray {
    if check_xray_running; then
        echo "Xray is running. Restarting..."
        pkill xray
        run_xray
    else
        echo "Xray is not running. Starting..."
        run_xray
    fi
}

# Initial Xray run
restart_xray

# Set up inotifywait to monitor file changes
inotifywait -m -e modify /home/andrew/Dropbox/xray/config.json | while read -r line; do
    echo "Config file modified. Restarting Xray..."
    restart_xray
done
