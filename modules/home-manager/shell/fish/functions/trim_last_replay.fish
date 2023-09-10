function trim_last_replay
    set filepath (find ~/Videos/ -name "*.mp4" -printf "%T@ %p\n" | sort -n | tail -n 1 | cut -d' ' -f2-)
    echo $filepath
    set trimmed_file_path (string replace -r '(.+)\.mp4$' '$1_trimmed.mp4' $filepath)
    # set trimmed_file_name (basename $trimmed_file_path)
    video-trimmer --output "$trimmed_file_path" $filepath
    if test -e $trimmed_file_path
        clip-file $trimmed_file_path
    end
end
