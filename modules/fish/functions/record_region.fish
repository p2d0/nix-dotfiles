function record_region
    if pgrep -x ffmpeg > /dev/null
        killall -s SIGINT ffmpeg
        killall -s SIGINT screenkey
        clip-file /tmp/output.mp4
        notify-send "Clipped"
    else
        notify-send "Started recording"
        ffmpeg (slop -r boxzoom -f "-y -video_size %wx%h -framerate 60 -f x11grab -i :0.0+%x,%y /tmp/output.mp4 -c:v hevc_v4l2m2" | string split -n " ")
        # available codecs:
        # vp9
    end
end
