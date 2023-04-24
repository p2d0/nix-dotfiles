function dnd -a on_or_off
    if test "on" = "$on_or_off"
        makoctl set-mode do-not-disturb
        echo "DND IS ON"
        notify-send "DND IS ON"
    else
        makoctl set-mode default
        echo "DND IS OFF"
        notify-send "DND IS OFF"
    end
end
