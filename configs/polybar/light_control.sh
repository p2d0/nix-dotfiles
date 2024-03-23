#!/usr/bin/env bash

# Change to the directory where the script is located
SCRIPT_DIR=$(dirname "$(realpath "$0")")
cd "$SCRIPT_DIR"

# Fetch brightness_and_temp and temperature from the python script
brightness_and_temp=$(python3 xiaomi_light_control.py display_both)

# Format the rofi prompt with brightness_and_temp and temperature
rofi_prompt="Yeelight Controls $brightness_and_temp"

# Options to be displayed in rofi
options=("Change Brightness" "Change Temperature" "Toggle Lights" "50% 3000K")

# Use rofi to select an option
selected_option=$(printf "%s\n" "${options[@]}" | rofi -dmenu -i -p "$rofi_prompt")

# Perform an action based on the selected option
case $selected_option in
    "Change Brightness")
        # Prompt for brightness_and_temp value and pass it to the python script
        brightness=$(rofi -dmenu -p "Enter brightness value in %: ")
        python3 xiaomi_light_control.py change $brightness
        ;;
    "Change Temperature")
        # Prompt for temperature value and pass it to the python script
        temperature=$(rofi -dmenu -p "Enter temperature value: ")
        python3 xiaomi_light_control.py temperature $temperature
        ;;
    "Toggle Lights")
        # Pass toggle command to the python script
        python3 xiaomi_light_control.py toggle
        ;;
    "50% 3000K")
        # Pass toggle command to the python script
        python3 xiaomi_light_control.py both 50 3000
        ;;
esac
