#!/usr/bin/env bash
pkill Hyprland
SINK="alsa_output.pci-0000_05_00.1.hdmi-stereo"  # Replace with your sink
SOUND_FILE="/etc/nixos/Homecoming.ogg"                 # Replace with your sound file
PID_FILE="/tmp/loop_sound.pid"                    # File to store the PID

# Store the PID of this script
echo $$ > "$PID_FILE"

# Infinite loop to play the sound
while true; do
    paplay --device="$SINK" "$SOUND_FILE"
done
