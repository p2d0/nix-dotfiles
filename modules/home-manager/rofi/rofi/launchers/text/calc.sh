#!/usr/bin/env bash


theme="style_3"

dir="$HOME/.config/rofi/launchers/text"
styles=($(ls -p --hide="colors.rasi" $dir/styles))
color="${styles[7]}"

rofi -no-lazy-grab -show calc \
-modi calc \
-theme $dir/"$theme" \
-calc-command 'wl-copy {result}'
