#!/usr/bin/env sh
sleep 1
setxkbmap us
for i in {1..10}; do
    xdotool type --clearmodifiers --delay 0 -- "/imagegen"
    sleep 0.001
    xdotool key Return
    sleep 0.001
    xdotool key Shift+Tab
done
setxkbmap -layout us,ru -option -option "grp:alt_shift_toggle";
