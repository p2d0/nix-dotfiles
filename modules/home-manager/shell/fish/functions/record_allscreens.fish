function record_allscreens
    # 1. Detect GPU Vendor
    set -l gpu_vendor (lspci | grep -i 'vga\|3d' | tr '[:upper:]' '[:lower:]')
    set -l codec "libx264"
    set -l device ""

    if string match -q "*amd*" "$gpu_vendor"; or string match -q "*intel*" "$gpu_vendor"
        set codec "h264_vaapi"
        set device "/dev/dri/renderD128"
    else if string match -q "*nvidia*" "$gpu_vendor"
        set codec "h264_nvenc"
    end

    # 2. Execution Logic
    if test "$XDG_SESSION_TYPE" = "wayland"
        if pgrep -x wf-recorder > /dev/null
            killall -s SIGINT wf-recorder
            clip-file /tmp/output.mp4
            notify-send "Clipped" "Recording saved to clipboard"
        else
            notify-send "Started recording" "Capturing all screens"
            
            if test "$codec" = "h264_vaapi"
                wf-recorder -y -c $codec -d $device -f /tmp/output.mp4 &
            else if test "$codec" = "h264_nvenc"
                wf-recorder -y -c $codec -x yuv420p -r 60 -f /tmp/output.mp4 &
            else
                wf-recorder -y -c $codec -f /tmp/output.mp4 &
            end
        end
    else
        # X11 Fallback
        if pgrep -x ffmpeg > /dev/null
            killall -s SIGINT ffmpeg
            clip-file /tmp/output.mp4
            notify-send "Clipped" "Recording saved to clipboard"
        else
            notify-send "Started recording" "Capturing full X11 desktop"
            # Using libx264 for generic X11 compatibility; capturing display :0.0
            ffmpeg -y -f x11grab -i :0.0 -framerate 60 -pix_fmt yuv420p -c:v libx264 -movflags +faststart /tmp/output.mp4 &
        end
    end
end
