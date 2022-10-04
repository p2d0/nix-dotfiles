function clip-files-in-current-dir -d "Copy file[s] to clipboard to paste to telegram,nautilus etc..."
    set -l list ();
    for file in $argv
        set -a list "file://$PWD/$file\n"
    end
    echo -e $list | xclip -sel clip -t text/uri-list -i
    # wl-copy -t text/uri-list if wayland
    # TODO check if wayland
end
