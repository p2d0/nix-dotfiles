#!/usr/bin/env sh

# Get current state
IS_OPEN=$(eww get show_panel)

# Toggle the state
if [ "$IS_OPEN" = "true" ]; then
    eww update show_panel=false
else
    eww update show_panel=true
fi
