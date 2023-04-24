function check_damiros
    while not test (curl "https://osu.ppy.sh/api/get_user_recent?k=b40b7a7a8207b1ebd870eaf1f74bd2995f1a2cb6&u=6440250" | jq length) -gt 0
        echo "Not playing yet";
        sleep 60;
    end
    notify-send "DAMIROS IS PLAYING!!!!!!!!!!!!"
    notify-send "DAMIROS IS PLAYING!!!!!!!!!!!!"
    notify-send "DAMIROS IS PLAYING!!!!!!!!!!!!"
    notify-send "DAMIROS IS PLAYING!!!!!!!!!!!!"
    notify-send "DAMIROS IS PLAYING!!!!!!!!!!!!"
    zenity --info --text="Damiros is playing"
end
