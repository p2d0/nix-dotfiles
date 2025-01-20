function trim_video_from_clipboard
    if test $XDG_SESSION_TYPE = "wayland"
        set filepath (wl-paste)
    else
        set filepath (xclip -selection clipboard -o)
    end
    set filepath (string sub -s 8 $filepath)
    echo $filepath;
    set trimmed_file_path (string replace -r '(.+)\.mp4$' '$1_trimmed.mp4' $filepath)
    # set trimmed_file_name (basename $trimmed_file_path)
    video-trimmer --output "$trimmed_file_path" $filepath
    if test -e $trimmed_file_path
        clip-file $trimmed_file_path
        end
end
