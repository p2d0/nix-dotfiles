function record_screen_replay_sound
    if test $XDG_SESSION_TYPE = "wayland"
    else
        notify-send "Started replay recording"
        gpu-screen-recorder (get_current_screen_geometry "-w {model} -c mp4 -q medium -f 60 -a recsink.monitor -v no -r 60 -o $HOME/Videos" | string split -n " ")
    end
end
