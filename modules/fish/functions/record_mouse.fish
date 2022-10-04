function record_mouse
    if pgrep -x ffmpeg > /dev/null
        killall -s SIGINT ffmpeg
        killall -s SIGINT screenkey
        clip-file /tmp/output.mp4
        notify-send "Clipped"
    else
        notify-send "Started recording"
        ffmpeg -y -video_size 600x600 -follow_mouse centered -framerate 24 -f x11grab -i :0.0  /tmp/output.mp4 -c:v h264_v4l2m2
    end
end
