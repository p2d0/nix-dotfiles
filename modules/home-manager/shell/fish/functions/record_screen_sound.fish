function record_screen_sound
    if test $XDG_SESSION_TYPE = "wayland"
        # if pgrep -x wf-recorder > /dev/null
        #     killall -s SIGINT wf-recorder
        #     # killall -s SIGINT screenkey
        #     clip-file /tmp/output.mp4
        #     notify-send "Clipped"
        # else
        #     notify-send "Started recording"
        #     echo "Y\n" | wf-recorder -g (swaymsg -t get_outputs | jq -r '.[] | select(.focused == true) | "\(.rect.x),\(.rect.y) \(.rect.width)x\(.rect.height)"') -c hevc_vaapi -d /dev/dri/renderD128 -x yuv420p -f /tmp/output.mp4
        # end
    else
        if pgrep -f gpu-screen-recorder > /dev/null
            killall -s SIGINT -f gpu-screen-recorder
            killall -s SIGINT screenkey
            clip-file /tmp/output.mp4
            notify-send "Clipped"
        else
            notify-send "Started recording"
            set sink (pactl get-default-sink)
            gpu-screen-recorder (get_current_screen_geometry "-w {model} -c mp4 -f 60 -a $sink.monitor -v no -o /tmp/output.mp4" | string split -n " ")
        end
    end
end