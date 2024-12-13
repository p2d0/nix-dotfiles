function record_screen_replay_sound
    if test $XDG_SESSION_TYPE = "wayland"
        if pgrep -f gpu-screen-recorder > /dev/null
            pkill -SIGUSR1 -f gpu-screen-recorder
            sleep 1
            set file (ls -lt $HOME/Videos/*.mp4 | head -n 1 | awk '{print $NF}')
            clip-file $file
            notify-send "Saved"
        else
            notify-send "Started replay recording"
            set sink (pactl get-default-sink)
            set src (pactl get-default-source)
            if not pactl list sinks | grep -q IOMix
                pactl load-module module-null-sink sink_name=IOMix sink_properties=device.description="input and output mixed"
                pactl load-module module-loopback source=$(pactl get-default-source) sink=IOMix
                pactl load-module module-loopback source=$(pactl get-default-sink).monitor sink=IOMix
            end
            gpu-screen-recorder -w (hyprctl monitors -j | jq -r '.[] | select(.focused == true) | .name') -c mp4 -ac aac -q medium -k h264 -f 60 -a IOMix.monitor -v no -r 60 -o $HOME/Videos
            # gpu-screen-recorder (get_current_screen_geometry "-w DP-1 -c mp4 -ac aac -q medium -k h264 -f 60 -a $sink.monitor|$src -v no -r 60 -o $HOME/Videos" | string split -n " ")
        end
    else
        if pgrep -f gpu-screen-recorder > /dev/null
            pkill -SIGUSR1 -f gpu-screen-recorder
            sleep 1
            set file (ls -lt $HOME/Videos/*.mp4 | head -n 1 | awk '{print $NF}')
            clip-file $file
            notify-send "Saved"
        else
            notify-send "Started replay recording"
            set sink (pactl get-default-sink)
            set src (pactl get-default-source)
            # pactl load-module module-null-sink sink_name=IOMix sink_properties=device.description="input and output mixed"
            # pactl load-module module-loopback source=$(pactl get-default-source) sink=IOMix
            # pactl load-module module-loopback source=$(pactl get-default-sink).monitor sink=IOMix
            gpu-screen-recorder (get_current_screen_geometry "-w DP-1 -c mp4 -ac aac -q medium -k h264 -f 60 -a $sink.monitor|$src -v no -r 60 -o $HOME/Videos" | string split -n " ")
        end
    end
end
