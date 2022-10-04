function record_allscreens
    if pgrep -x ffmpeg > /dev/null
        killall -s SIGINT ffmpeg
        killall -s SIGINT screenkey
        clip-file /tmp/output.mp4
        notify-send "Clipped"
    else
        notify-send "Started recording"
        ffmpeg (get_current_screen_geometry "-y -framerate 24 -f x11grab -i :0.0 -vf scale=-1:720 /tmp/output.mp4 -c:v h264_v4l2m2" | string split -n " ")
    end
end
