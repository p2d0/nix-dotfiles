#!/usr/bin/env python
# Import the necessary modules
from miio import Yeelight
from miio.exceptions import DeviceException
from sys import argv
from time import sleep
import os

def ping(host):
    response = os.system("ping -c 1 " + host + " > /dev/null 2>&1")

    if response == 0:
        return True
    else:
        return False

# Define the lightbulbs data as a list of dictionaries
yeelights = [
    {"ip": "192.168.31.101", "token": "67c3d12c44ada627ad6e37a4db561f8d", "model": "yeelink.light.color5"},
    {"ip": "192.168.31.247", "token": "aae89b7b5e2560d50e9877026a2f9ba5", "model": "yeelink.light.color5"},
    {"ip": "192.168.31.144", "token": "80e52c76c607d227190857502638ca6e", "model": "yeelink.light.color5"}
]

def create_yeelight(lightbulb):
    ping(lightbulb["ip"])
    yeelight = Yeelight(lightbulb["ip"], lightbulb["token"], model=lightbulb["model"])
    yeelight.timeout = 5000;
    return yeelight

# Define a function to get the brightness of a lightbulb
def get_brightness(lightbulb):
    yeelight = create_yeelight(lightbulb)
    brightness = yeelight.get_properties(["bright"])[0]
    return int(brightness)

# Define a function to set the brightness of a lightbulb
def set_brightness(lightbulb, brightness):
    try:
        yeelight = create_yeelight(lightbulb)
        yeelight.set_brightness(brightness)
        updated_brightness = get_brightness(lightbulb)
    except DeviceException:
        sleep(0.5)
        set_brightness(lightbulb, brightness)
    if(updated_brightness != brightness):
        sleep(0.5)
        set_brightness(lightbulb, brightness)

# Define a function to set the color temperature of a lightbulb
def set_temperature(lightbulb, temperature):
    try:
        yeelight = create_yeelight(lightbulb)
        yeelight.set_color_temp(temperature)
        updated_temperature = int(yeelight.get_properties(["ct"])[0])
    except DeviceException:
        sleep(0.5)
        set_temperature(lightbulb, temperature)
    if(updated_temperature != temperature):
        sleep(0.5)
        set_temperature(lightbulb, temperature)

# Define a function to get the power status of a lightbulb
def get_power_status(lightbulb):
    yeelight = create_yeelight(lightbulb)
    power_status = yeelight.get_properties(["power"])[0]
    return power_status

# Define a function to toggle a lightbulb on and off
def toggle_light(lightbulb):
    yeelight = create_yeelight(lightbulb)
    try:
        current_state = yeelight.get_properties(["power"])[0]
        yeelight.toggle()  # Toggle the light state
    except DeviceException:
        sleep(0.5);
        toggle_light(lightbulb)
    new_state = yeelight.get_properties(["power"])[0]
    print(new_state,current_state);
    if(current_state == new_state):
        sleep(0.5);
        toggle_light(lightbulb)
    return current_state, new_state

def enable_light(lightbulb):
    yeelight = create_yeelight(lightbulb)
    try:
        yeelight.on()  # Toggle the light state
    except DeviceException:
        sleep(0.5);
        enable_light(lightbulb)

# Define a function to display the brightness of the lightbulbs
def display_brightness():
    output = ""
    lightbulb = yeelights[0]
    power_status = get_power_status(lightbulb)

    if power_status == "on":
        brightness = get_brightness(lightbulb)
        output += f"{brightness}%"
    else:
        output += "off"

    print(output)

# Define a function to display the temperature of the lightbulbs
def display_temperature():
    output = ""
    lightbulb = yeelights[0]
    power_status = get_power_status(lightbulb)

    if power_status == "on":
        yeelight = create_yeelight(lightbulb)
        temperature = yeelight.get_properties(["ct"])[0]
        output += f"{temperature}K"
    else:
        output += "off"

    print(output)

# Define a function to change the brightness of the lightbulbs by a given amount
def change_brightness(amount):
    for lightbulb in yeelights:
        brightness = get_brightness(lightbulb)
        brightness = min(max(brightness + amount, 1), 100)
        set_brightness(lightbulb, brightness)
    print(f"Brightness changed by {amount}%")

def set_brightness_for_all(brightness):
    for lightbulb in yeelights:
        set_brightness(lightbulb, brightness)
    print(f"Brightness changed by {amount}%")

# Define a function to change the temperature of the lightbulbs
def change_temperature(amount):
    for lightbulb in yeelights:
        yeelight = create_yeelight(lightbulb)
        current_temperature = int(yeelight.get_properties(["ct"])[0])
        new_temperature = min(max(current_temperature + amount, 1000), 6500)
        set_temperature(lightbulb, new_temperature)
    print(f"Temperature changed by {amount}K")

# Define a function to toggle all lightbulbs
def toggle_lights():
    states = [toggle_light(lightbulb) for lightbulb in yeelights]
    for i, (current_state, new_state) in enumerate(states):
        print(f"Light {i + 1} toggled: {current_state} -> {new_state}")

def enable_lights():
    states = [enable_light(lightbulb) for lightbulb in yeelights]
    # for i in enumerate(states):
    #     print(f"Light {i + 1} toggled: {current_state} -> {new_state}")

# Check if the script is run with the correct number of arguments
if len(argv) > 2:
    arg1 = argv[1]
    arg2 = int(argv[2])

    if arg1 == "change":
        change_brightness(arg2)
    if arg1 == "set_brightness":
        set_brightness_for_all(arg2)
    elif arg1 == "temperature":
        change_temperature(arg2)
    else:
        print("Invalid command")
elif len(argv) > 1:
    arg1 = argv[1]
    if arg1 == "display_brightness":
        display_brightness()
    elif arg1 == "display_temp":
        display_temperature()
    elif arg1 == "toggle":
        toggle_lights()
    elif arg1 == "enable":
        enable_lights()
else:
    print("Usage: python polybar_yeelight.py <display_brightness|display_temp|change|temperature|toggle> <amount|temperature>")
