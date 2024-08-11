#!/usr/bin/env sh

# Check if the file /tmp/emergency does not exist
if [ ! -e /tmp/emergency ]; then
    # If the file does not exist, shut down the system
    shutdown -h now
else
    # If the file exists, do nothing
    echo "File /tmp/emergency exists. No action taken."
fi
