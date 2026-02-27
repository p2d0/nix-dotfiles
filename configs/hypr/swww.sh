#!/usr/bin/env sh

if [ "$(darkman get)" = "dark" ]; then
    swww img /etc/nixos/bg_old.png --transition-type center --transition-fps 75
else
    swww img /etc/nixos/light.jpg --transition-type center --transition-fps 75
fi
