#!/usr/bin/env python
# Import the necessary modules
from miio import Yeelight
from sys import argv

# Define the lightbulbs data as a list of dictionaries
yeelights = [
    {"ip": "192.168.31.101", "token": "67c3d12c44ada627ad6e37a4db561f8d", "model": "yeelink.light.color5"},
    {"ip": "192.168.31.247", "token": "aae89b7b5e2560d50e9877026a2f9ba5", "model": "yeelink.light.color5"},
    {"ip": "192.168.31.144", "token": "80e52c76c607d227190857502638ca6e", "model": "yeelink.light.color5"}
]

# Define a function to get the brightness of a lightbulb
def get_brightness(lightbulb):
    # Create a Yeelight object with the given ip, token and model
    yeelight = Yeelight(lightbulb["ip"], lightbulb["token"], model=lightbulb["model"])
    # Get the brightness of the lightbulb as an integer
    brightness = yeelight.get_properties(["bright"])[0]
    # Return the brightness value
    return int(brightness)

# Define a function to set the brightness of a lightbulb
def set_brightness(lightbulb, brightness):
    # Create a Yeelight object with the given ip, token and model
    yeelight = Yeelight(lightbulb["ip"], lightbulb["token"], model=lightbulb["model"])
    # Set the brightness of the lightbulb to the given value
    yeelight.set_brightness(brightness)

# Define a function to display the brightness of the lightbulbs
def display_brightness():
    # Initialize an empty string to store the output
    output = ""
    # Loop through the lightbulbs and get their brightness
    lightbulb  = yeelights[0]
    brightness = get_brightness(lightbulb)
    # Append the brightness value to the output string
    output += f"{brightness}% "
    # Print the output string
    print(output)

# Define a function to change the brightness of the lightbulbs by a given amount
def change_brightness(amount):
    # Loop through the lightbulbs and get their brightness
    for lightbulb in yeelights:
        brightness = get_brightness(lightbulb)
        # Add the amount to the brightness value and clamp it to the range [1, 100]
        brightness = min(max(brightness + amount, 1), 100)
        # Set the brightness of the lightbulb to the new value
        set_brightness(lightbulb, brightness)
    # Print a message to indicate the operation is done
    print(f"Brightness changed by {amount}%")

# Check if the script is run with two arguments

if len(argv) > 2:
    # Get the first argument value
    arg1 = argv[1]
    # Get the second argument value as an integer
    arg2 = int(argv[2])
    # If the first argument is "display", call the display_brightness function
    if arg1 == "display":
        display_brightness()
    # If the first argument is "change", call the change_brightness function with the second argument as the amount
    elif arg1 == "change":
        change_brightness(arg2)
    # Otherwise, print a message to indicate the usage of the script
elif len(argv) > 1:
    arg1 = argv[1]
    if arg1 == "display":
        display_brightness()
else:
    # Print a message to indicate the usage of the script
    print("Usage: python polybar_yeelight.py <display|change> <amount>")
