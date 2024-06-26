function record_mouse_big
    if pgrep -x ffmpeg > /dev/null
        killall -s SIGINT ffmpeg
        killall -s SIGINT screenkey
        clip-file /tmp/output.mp4
        notify-send "Clipped"
    else
        notify-send "Started recording"
        echo "pepega"
        ffmpeg -y -video_size 1280x720  -follow_mouse centered -framerate 24 -f x11grab -i :0.0 -pix_fmt yuv420p /tmp/output.mp4 -c:v h264_v4l2m2m -movflags +faststart
    end
end
