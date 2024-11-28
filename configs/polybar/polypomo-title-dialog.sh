#!/usr/bin/env bash

# Open a zenity dialog to input the task title
title=$(zenity --entry --text="Enter the task title:" --entry-text="")

# Check if the user entered a title and pass it to polypomo
if [ -n "$title" ]; then
    ~/.config/polybar/polypomo title "$title"
else
    ~/.config/polybar/polypomo title ""
fi