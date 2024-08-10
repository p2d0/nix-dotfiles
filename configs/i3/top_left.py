#!/usr/bin/env python
import subprocess
from dataclasses import dataclass
import re

GREP = re.compile(r"^(?P<name>[^ ]+) .* (?P<width>\d+)x(?P<height>\d+)\+(?P<x>\d+)\+(?P<y>\d+).*$")


@dataclass
class Monitor:
	name: str
	width: int
	height: int
	x: int
	y: int

	def contains(self, x, y):
		return self.x <= x <= self.x + self.width and self.y <= y <= self.y + self.height


def main():
	mx, my = None, None
	output = subprocess.check_output(["xdotool", "getmouselocation", "--shell"]).decode().split("\n")
	for line in output:
		line = line.split("=")

		match line[0]:
			case "X":
				mx = int(line[1])
			case "Y":
				my = int(line[1])

	print(f"mx = {mx}, my = {my}")
	if mx is None or my is None:
		return

	lines = subprocess.check_output(["xrandr", "--current"]).decode().split("\n")
	lines = filter(lambda line: " connected " in line, lines)

	for line in lines:
		gd = GREP.match(line).groupdict()
		print(f"Detected: {gd}")

		monitor = Monitor(name=gd["name"], width=int(gd["width"]), height=int(gd["height"]), x=int(gd["x"]), y=int(gd["y"]))

		if monitor.contains(mx, my):
			print(f"Current monitor: {monitor.name}")
			print(
				subprocess.check_output(["i3-msg", f'[instance="guake"] move position {monitor.x} px {monitor.y} px']).decode()
			)
			break
	else:
		print("Impossible...")

if __name__ == '__main__':
	main()
