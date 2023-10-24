#!/bin/sh

YAD_WIDTH=222  # 222 is minimum possible value
YAD_HEIGHT=193 # 193 is minimum possible value
DATE="$(date +"%a %d %H:%M")"

case "$1" in
--popup)
    yad --calendar --undecorated --fixed --close-on-unfocus --no-buttons --mouse --posy=-30 \
        --title="calendar" --borders=0 > /dev/null &
    ;;
*)
    echo "%{F#46b9d6}%{F-}" $(date "+%H:%M %a, %d %b")
    ;;
esac
