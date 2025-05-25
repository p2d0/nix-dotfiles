function flameshot_hyprland --description "Save cursor position when launching flameshot on Hyprland"
    # Launch flameshot in background
    flameshot gui &

    set mouse_pos (hyprctl cursorpos | string split -n ", ")

    # Wait until a client with class "flameshot" appears
    while not hyprctl clients -j | jq -e '.[] | select(.class == "flameshot")' > /dev/null
        sleep 0.05
    end

    hyprctl dispatch movecursor $mouse_pos[1] $mouse_pos[2]
end
