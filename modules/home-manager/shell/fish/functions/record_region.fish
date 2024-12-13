function record_region
    if test $XDG_CURRENT_DESKTOP=hyprland
        if test $XDG_SESSION_TYPE = "wayland"
            if pgrep -x wf-recorder > /dev/null
                killall -s SIGINT wf-recorder
                # killall -s SIGINT screenkey
                clip-file /tmp/output.mp4
                notify-send "Clipped"
            else
                notify-send "Started recording"
                echo "Y\n" | wf-recorder -g (slurp) -c h264_nvenc -x yuv420p -r 60 -f /tmp/output.mp4
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
                    echo "Y\n" | wf-recorder -g (slurp) -c h264_vaapi -F 'hwupload,scale_vaapi=format=yuv420p' -d /dev/dri/renderD128 -f /tmp/output.mp4
                end
            else
                if pgrep -x ffmpeg > /dev/null
                    killall -s SIGINT ffmpeg
                    killall -s SIGINT screenkey
                    clip-file /tmp/output.mp4
                    notify-send "Clipped"
                else
                    notify-send "Started recording"
                    ffmpeg (slop -r boxzoom -f "-y -video_size %wx%h -framerate 60 -f x11grab -i :0.0+%x,%y /tmp/output.mp4 -pix_fmt yuv420p -c:v h264_v4l2m2 -movflags +faststart" | string split -n " ")
                    # available codecs:
                    # vp9
                end
            end
        end
    end
