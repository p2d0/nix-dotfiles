function y
    for path in (cat "$HOME/tracked_folders" | envsubst | string split "\n")
        yadm add $path
    end
    yadm add -u
    test (count $argv) -eq 0 && yadm status || yadm "$argv"
end
