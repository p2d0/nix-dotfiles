function clip-file -d "Copy file[s] to clipboard to paste to telegram,nautilus etc..."
    if test $XDG_SESSION_TYPE = "wayland"
        set copier wl-copy -t text/uri-list
    else
        set copier xclip -sel clip -t text/uri-list -i
    end

    echo -e "file://$argv[1]" | $copier
end
