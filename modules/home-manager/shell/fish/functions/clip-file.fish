function clip-file -d "Copy file[s] to clipboard to paste to telegram,nautilus etc..."
    # Check if the argument is a valid file path
    if not test -f $argv[1]
        echo "Invalid file path: $argv[1]"
        return 1
    end

    # Expand the argument to a full file path
    set full_path (realpath $argv[1])

    # Use the appropriate copier command based on the session type
    if test $XDG_SESSION_TYPE = "wayland"
        set copier wl-copy -t text/uri-list
    else
        set copier xclip -sel clip -t text/uri-list -i
    end

    # Copy the file URL to clipboard
    echo -e "file://$full_path" | $copier
end
