function record_allscreens
    if test $XDG_CURRENT_DESKTOP=hyprland
            if pgrep -x wf-recorder > /dev/null
                killall -s SIGINT wf-recorder
                # killall -s SIGINT screenkey
                clip-file /tmp/output.mp4
                notify-send "Clipped"
            else
                notify-send "Started recording"
                echo "Y\n" | wf-recorder -c h264_nvenc -x yuv420p -r 60 -f /tmp/output.mp4
            end
    else
        if test $XDG_SESSION_TYPE = "wayland"
            if pgrep -x wf-recorder > /dev/null
                killall -s SIGINT wf-recorder
                # killall -s SIGINT screenkey
                clip-file /tmp/output.mp4
                notify-send "Clipped"
            else
                notify-send "Started recording"
                echo "Y\n" | wf-recorder -c h264_vaapi -d /dev/dri/renderD128  -f /tmp/output.mp4
            end
        else
            if pgrep -x ffmpeg > /dev/null
                killall -s SIGINT ffmpeg
                killall -s SIGINT screenkey
                clip-file /tmp/output.mp4
                notify-send "Clipped"
            else
                notify-send "Started recording"
                ffmpeg (get_current_screen_geometry "-y -framerate 24 -f x11grab -i :0.0 -vf scale=-1:720 -pix_fmt yuv420p /tmp/output.mp4 -c:v h264_v4l2m2 -movflags +faststart" | string split -n " ")
            end
        end
    end
end
