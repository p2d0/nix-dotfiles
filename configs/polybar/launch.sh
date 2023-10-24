#!/usr/bin/env bash
WORK_MODE=0
source /etc/set-environment
# Terminate already running bar instances
killall -q polybar
pkill polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

# Launch bars
if [ "$WORK_MODE" = "1" ]; then
        polybar -r workdvi &
        polybar -r workbar &
    else
        polybar -r DVI &
        polybar -r mybar &
fi
