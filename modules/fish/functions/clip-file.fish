function clip-file -d "Copy file[s] to clipboard to paste to telegram,nautilus etc..."
    echo -e "file://$argv[1]" | xclip -sel clip -t text/uri-list -i
end
