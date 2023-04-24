function fdd
    set -l dir (fd --type d --hidden . | peco --query "$argv")
    if test -n "$dir"
        cd "$dir"
    end
end
