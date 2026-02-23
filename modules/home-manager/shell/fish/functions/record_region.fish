function record_region
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
            notify-send "KILLING SHIT"
            killall -s SIGINT wf-recorder
            clip-file /tmp/output.mp4
            notify-send "Clipped"
        else
            set -l geom (slurp)
            if test -z "$geom"
                return
            end

            notify-send "Started recording"
            if test "$codec" = "h264_vaapi"
                wf-recorder -y -g "$geom" -c $codec -d $device -f /tmp/output.mp4 &
            else if test "$codec" = "h264_nvenc"
                wf-recorder -y -g "$geom" -c $codec -x yuv420p -r 60 -f /tmp/output.mp4 &
            else
                wf-recorder -y -g "$geom" -c $codec -f /tmp/output.mp4 &
            end
        end
    else
        # X11 Fallback
        if pgrep -x ffmpeg > /dev/null
            killall -s SIGINT ffmpeg
            clip-file /tmp/output.mp4
            notify-send "Clipped"
        else
            notify-send "Started recording"
            # We use libx264 for X11/ffmpeg for stability, or could use h264_nvenc if ffmpeg is compiled with it
            ffmpeg (slop -r boxzoom -f "-y -video_size %wx%h -framerate 60 -f x11grab -i :0.0+%x,%y /tmp/output.mp4 -pix_fmt yuv420p -c:v libx264 -movflags +faststart" | string split -n " ") &
        end
    end
end
