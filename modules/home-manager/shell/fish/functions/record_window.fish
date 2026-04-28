function record_window
    # 1. Detect GPU Vendor
    set -l gpu_vendor (lspci | grep -i 'vga\|3d' | tr '[:upper:]' '[:lower:]')
    set -l codec_flags ""

    if string match -q "*amd*" "$gpu_vendor"; or string match -q "*intel*" "$gpu_vendor"
        set codec "h264_vaapi"
        set device "/dev/dri/renderD128"
    else if string match -q "*nvidia*" "$gpu_vendor"
        set codec "h264_nvenc"
        set device ""
    end

    # 2. Execution Logic
    if test "$XDG_CURRENT_DESKTOP" = "Hyprland"
        if pgrep -x wf-recorder > /dev/null
            killall -s SIGINT wf-recorder
            clip-file /tmp/output.mp4
            notify-send "Recording Saved" "Clipped to clipboard"
        else
            # Get geometry for active window in Hyprland
            set -l geom (hyprctl activewindow -j | jq -r '"\(.at[0]),\(.at[1]) \(.size[0])x\(.size[1])"')

            if test -z "$geom"
                notify-send "No focused window found"
                return
            end

            notify-send "Recording Started" "Capturing focused window"
            if test -n "$device"
                wf-recorder -y -g "$geom" -c $codec -d $device -f /tmp/output.mp4 &
            else
                wf-recorder -y -g "$geom" -c $codec -f /tmp/output.mp4 &
            end
        end
    else
        echo "record_window only supports Hyprland"
    end
end
