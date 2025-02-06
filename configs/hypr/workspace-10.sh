#!/usr/bin/env sh


emacsclient -c -F "'(name . \"emacs-todo\"))" --eval "(emacs-todo)" &
brave --new-window --app=https://calendar.notion.so/

    # && sleep 0.9 \
    # && hyprctl dispatch resizewindowpixel exact 25% 100%, class:brave-calendar.google.com__-Default \
    # && hyprctl dispatch hy3:movewindow left
