function record_screen_replay_sound_stop
    if pgrep -f gpu-screen-recorder > /dev/null
        pkill -SIGINT -f gpu-screen-recorder
        notify-send "Stopped"
    else
        notify-send "Not recording"
    end
end
