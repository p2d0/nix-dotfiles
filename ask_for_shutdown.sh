#!/bin/bash
# This script asks the user yes or no and shuts down the system if the user answers yes

# Display a dialog box with yes and no buttons
answer=$(zenity --question --text="Time to wind-down. Shut down now?" --ok-label="Yes" --cancel-label="No")

# Check the exit status of zenity
if [ $? -eq 0 ]; then
    # The user answered yes, run the shutdown command
    shutdown -h now
else
    # The user answered no, do nothing
    exit 0
fi
