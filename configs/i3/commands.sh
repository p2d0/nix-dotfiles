#!/usr/bin/env sh
ACTION=$(echo -e "REMOVE TITLEBAR\nSTOP FULLSCREEN" | rofi -i -dmenu -no-custom -p "Select action")

if [ -z "$ACTION" ]; then
    exit
fi

if [ "$ACTION" == "REMOVE TITLEBAR" ]; then
    i3-msg border none
fi

if [ "$ACTION" == "STOP FULLSCREEN" ]; then
    i3-msg fullscreen disable
fi
