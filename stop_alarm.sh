#!/usr/bin/env bash
PID_FILE="/tmp/loop_sound.pid"

# Check if PID file exists
if [ -f "$PID_FILE" ]; then
    PID=$(cat "$PID_FILE")
    if [ -n "$PID" ] && kill -0 "$PID" 2>/dev/null; then
        kill "$PID" && echo "Sound loop stopped."
        pkill paplay
        pkill aplay
        rm -f "$PID_FILE"  # Clean up PID file
    else
        echo "No running sound loop found."
        rm -f "$PID_FILE"  # Clean up stale PID file
    fi
else
    echo "No sound loop is running (PID file not found)."
fi
