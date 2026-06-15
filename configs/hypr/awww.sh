#!/usr/bin/env sh

if [ "$(darkman get)" = "dark" ]; then
    awww img /etc/nixos/bg_old.png --transition-type center --transition-fps 75
else
    awww img /etc/nixos/light.jpg --transition-type center --transition-fps 75
fi
