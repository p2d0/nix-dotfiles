function record_screen
    if test $XDG_SESSION_TYPE = "wayland"
        if pgrep -x wf-recorder > /dev/null
            killall -s SIGINT wf-recorder
            # killall -s SIGINT screenkey
            clip-file /tmp/output.mp4
            notify-send "Clipped"
        else
            notify-send "Started recording"
            echo "Y\n" | wf-recorder -g (swaymsg -t get_outputs | jq -r '.[] | select(.focused == true) | "\(.rect.x),\(.rect.y) \(.rect.width)x\(.rect.height)"') -c h264_vaapi -d /dev/dri/renderD128 -f /tmp/output.mp4
        end
    else
        if pgrep -x ffmpeg > /dev/null
            killall -s SIGINT ffmpeg
            killall -s SIGINT screenkey
            clip-file /tmp/output.mp4
            notify-send "Clipped"
        else
            notify-send "Started recording"
            ffmpeg (get_current_screen_geometry "-y -video_size {width}x{height} -framerate 30  -f x11grab -i :0.0+{x},{y}  -vf scale=1280:-1  -pix_fmt yuv420p  /tmp/output.mp4 -pix_fmt yuv420p -c:v h264_v4l2m2 -movflags +faststart" | string split -n " ")
        end
    end
end
