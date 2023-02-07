#!/bin/sh

B='#00000000'  # blank
C='#ffffff22'  # clear ish
D='#ff00ffcc'  # default
T='#ee00eeee'  # text
W='#880000bb'  # wrong
V='#bb00bbbb'  # verifying
White='#ffffffff'
Black='#000000FF'

i3lock \
--timecolor=$White        \
--datecolor=$White        \
--screen 1            \
--color $Black             \
--clock               \
--indicator           \
--timestr="%H:%M:%S"  \
--datestr="%A, %m %Y" \
--keylayout 1
# --insidevercolor=$C   \
# --ringvercolor=$V     \
# \
# --insidewrongcolor=$C \
# --ringwrongcolor=$W   \
# \
# --insidecolor=$B      \
# --ringcolor=$D        \
# --linecolor=$B        \
# --separatorcolor=$D   \
# \
# --verifcolor=$T        \
# --wrongcolor=$T        \
# --layoutcolor=$T      \
# --keyhlcolor=$W       \
# --bshlcolor=$W        \
# \

# --veriftext="Drinking verification can..."
# --wrongtext="Nope!"
# --textsize=20
# --modsize=10
# --timefont=comic-sans
# --datefont=monofur
# etc
