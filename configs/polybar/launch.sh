#!/usr/bin/env bash

# Terminate already running bar instances
killall -q polybar
pkill polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

# Launch bars
polybar -r DVI &
polybar -r mybar &
