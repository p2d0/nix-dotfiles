#!/usr/bin/env bash
choice=$(echo -e '\nexit\nshutdown\nreboot' | dmenu -i -p 'Choose option' || exit 1);
case $choice in
    exit)  i3-msg $choice;;
    shutdown) shutdown now;;
    reboot) reboot;;
esac
