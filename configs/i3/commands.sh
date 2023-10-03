#!/usr/bin/env sh
ACTION=$(echo "REMOVE TITLEBAR" | rofi -i -dmenu -no-custom -p "Select action")

if [ -z "$ACTION" ]; then
    exit
fi

if [ "$ACTION" == "REMOVE TITLEBAR" ]; then
    i3-msg border none
fi
