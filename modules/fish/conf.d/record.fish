function record
    if pgrep -x ffmpeg > /dev/null
        pkill -SIGINT ffmpeg
        pkill -SIGINT screenkey
        clip-file /tmp/output.mp4
        notify-send "Clipped"
    else
        set theme "style_7"
        set dir "$HOME/.config/rofi/launchers/colorful"
        set choice (rofi -i -p "Record:" -input "$HOME/.config/fish/conf.d/choices.txt" -format 'i' -dmenu -theme $dir/"$theme")
        switch $choice
            case 0
                record_region;
            case 1
                record_screen;
            case 2
                record_allscreens;
            case 3
                record_mouse;
            case 4
                record_mouse_big;
        end
    end
end
