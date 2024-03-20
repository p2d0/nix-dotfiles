import sys
sys.path.append('../')
from xiaomi_light_control import get_brightness_and_temperature, yeelights

def test_main():
    assert "00" in get_brightness_and_temperature()
