function record
    if test $XDG_SESSION_TYPE = "wayland"
        set command_to_kill "wf-recorder"
    else
        set command_to_kill "ffmpeg"
    end

    #TODO deduplicate

    if pgrep -x $command_to_kill > /dev/null
        pkill -SIGINT $command_to_kill
        pkill -SIGINT screenkey
        clip-file /tmp/output.mp4
        notify-send "Clipped"
    else if pgrep -f gpu-screen-recorder > /dev/null
        pkill -SIGINT -f gpu-screen-recorder
        pkill -SIGINT screenkey
        clip-file /tmp/output.mp4
        notify-send "Clipped"
    else
        set theme "style_7"
        set dir "$HOME/.config/rofi/launchers/colorful"
        if test $XDG_SESSION_TYPE = "wayland"
            set monitor (swaymsg -t get_outputs | jq 'map(.focused) | index(false)')
            set choice (rofi -i -monitor $monitor  -p "Record:" -input "$HOME/.config/fish/functions/choices.txt" -format 'i' -dmenu -theme $dir/"$theme")
        else
            set choice (rofi -i -p "Record:" -input "$HOME/.config/fish/functions/choices.txt" -format 'i' -dmenu -theme $dir/"$theme")
        end
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
            case 5
                record_screen_sound;
            case 6
                record_screen_replay_sound;
            case 6
                record_screen_replay_sound_stop;
        end
    end
end
