#!/usr/bin/env bash
DEVICE="hw:0,3"                                   # Your ALSA device
SOUND_FILE="/etc/nixos/homecoming_40.wav"                 # Your sound file
PID_FILE="/tmp/loop_sound.pid"

# Store the PID
echo $$ > "$PID_FILE"

# Loop the sound with volume control
while true; do
    aplay -D "$DEVICE" "$SOUND_FILE"
done
