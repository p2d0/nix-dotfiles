#!/usr/bin/env bash

# Function to run Xray
function run_xray {
    xray run -c /home/andrew/Dropbox/xray/config.json
}

# Function to check file modification time
function check_file_modified {
    file="/home/andrew/Dropbox/xray/config.json"
    last_modified=$(stat -c %Y "$file")
    if [[ $last_modified -gt $previous_modified ]]; then
        return 0  # File has been modified
    else
        return 1
    fi
}

# Initial Xray run
run_xray

# Set up inotifywait to monitor file changes
inotifywait -m -e modify /home/andrew/Dropbox/xray/config.json | while read -r line; do
    if check_file_modified; then
        echo "Config file modified. Restarting Xray..."
        run_xray
        previous_modified=$(stat -c %Y "$file")
    fi
done
