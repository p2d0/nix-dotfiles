function clip-files-in-current-dir -d "Copy file[s] to clipboard to paste to telegram,nautilus etc..."
    set -l list ();
    for file in $argv
        set -a list "file://$PWD/$file\n"
    end
    if test $XDG_SESSION_TYPE = "wayland"
        set copier wl-copy -t text/uri-list
    else
        set copier xclip -sel clip -t text/uri-list -i
    end

    echo -e $list | $copier
    # wl-copy -t text/uri-list if wayland
    # TODO check if wayland
end

