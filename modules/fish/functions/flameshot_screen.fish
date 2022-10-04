function flameshot_screen
    flameshot (get_current_screen_geometry "full --region={width}x{height}+{x}+{y} -c --delay=1000" | string split -n " ")
end
