# set hour (date +%H)
# if test $hour -lt 9;
    # shutdown now
# end
set TTY1 (tty)
if test -z "$DISPLAY"; and test $TTY1 = "/dev/tty1"
startx
  #exec sway

end
