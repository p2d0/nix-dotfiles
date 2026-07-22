function record_screen
    # 1. Detect GPU Vendor and VRAM
    set -l gpu_vendor (lspci | grep -i 'vga\|3d' | tr '[:upper:]' '[:lower:]')
    set -l codec ""
    set -l device ""

    if string match -q "*amd*" "$gpu_vendor"; or string match -q "*intel*" "$gpu_vendor"
        set codec "h264_vaapi"
        set device "/dev/dri/renderD128" # Usually D128 for the primary GPU

    else if string match -q "*nvidia*" "$gpu_vendor"
        # Check available VRAM in MiB if nvidia-smi exists
        if command -q nvidia-smi
            set -l free_vram (nvidia-smi --query-gpu=memory.free --format=csv,noheader,nounits | head -n 1 | string trim)
            
            # Compare available VRAM against the 500MB threshold
            if test -n "$free_vram"; and test "$free_vram" -gt 500
                set codec "h264_nvenc"
                set device "" # NVENC doesn't use the -d flag in wf-recorder
            else
                set codec "libx264" # Fallback to CPU if VRAM < 500MB
            end
        else
            set codec "libx264"
        end
    else
        # Fallback for non-detected hardware
        set codec "libx264"
    end


    # 2. Execution Logic
    if test "$XDG_CURRENT_DESKTOP" = "Hyprland"
        if pgrep -x wf-recorder > /dev/null
            killall -s SIGINT wf-recorder
            clip-file /tmp/output.mp4
            notify-send "Recording Saved" "Clipped to clipboard"
        else
            notify-send "Recording Started" "Capturing with $codec"
            set -l geom (hyprctl monitors -j | jq -r '.[] | select(.focused == true) | "\(.x),\(.y) \(.width)x\(.height)"')
            
            if test -n "$device"
                wf-recorder -y -g "$geom" -c $codec -d $device -f /tmp/output.mp4 &
            else
                wf-recorder -y -g "$geom" -c $codec -f /tmp/output.mp4 &
            end
        end

    else if test "$XDG_SESSION_TYPE" = "wayland"
        if pgrep -x wf-recorder > /dev/null
            killall -s SIGINT wf-recorder
            clip-file /tmp/output.mp4
            notify-send "Clipped"
        else
            notify-send "Started recording"
            set -l geom (swaymsg -t get_outputs | jq -r '.[] | select(.focused == true) | "\(.rect.x),\(.rect.y) \(.rect.width)x\(.rect.height)"')
            
            wf-recorder -y -g "$geom" -c $codec -f /tmp/output.mp4 &
        end

    else
        # X11 Fallback using ffmpeg
        if pgrep -x ffmpeg > /dev/null
            killall -s SIGINT ffmpeg
            clip-file /tmp/output.mp4
            notify-send "Clipped"
        else
            notify-send "Started recording"
            ffmpeg (get_current_screen_geometry "-y -video_size {width}x{height} -framerate 30 -f x11grab -i :0.0+{x},{y} -c:v $codec -preset ultrafast -pix_fmt yuv420p /tmp/output.mp4") &
        end
    end
end
