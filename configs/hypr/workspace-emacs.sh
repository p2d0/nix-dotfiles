#!/usr/bin/env sh

emacsclient -c -F "'(name . \"emacs-todo\"))" --eval "(emacs-todo)" &
brave --disable-features=WaylandWpColorManagerV1 --new-window --app=https://calendar.notion.so/
