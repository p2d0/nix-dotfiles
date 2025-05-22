#!/usr/bin/env sh

emacsclient -c -F "'(name . \"emacs-todo\"))" --eval "(emacs-todo)" &
brave --new-window --app=https://calendar.notion.so/
