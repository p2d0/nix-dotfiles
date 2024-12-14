function flameshot_screen
    if test $XDG_CURRENT_DESKTOP=hyprland
        set -q XDG_CURRENT_DESKTOP=sway
        set -q USE_WAYLAND_GRIM=sway
        grim -g (hyprctl monitors -j | jq -r '.[] | select(.focused == true) | "\(.x),\(.y) \(.width)x\(.height)"') - | wl-copy -t image/png
        notify-send "Captured"
    else
        # flameshot (get_current_screen_geometry "full --region={width}x{height}+{x}+{y} -c --delay=1000" | string split -n " ")
    end
end
