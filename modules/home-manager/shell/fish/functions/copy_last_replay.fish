function copy_last_replay
    set filepath (find ~/Videos/ -name "*.mp4" -printf "%T@ %p\n" | sort -n | tail -n 1 | cut -d' ' -f2-)
    clip-file $filepath;
    notify-send "Clipped";
end
