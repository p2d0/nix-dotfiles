function llm_folder --description "Slurp files to /tmp/llm.txt, estimate tokens, and copy"
    set -l output_file /tmp/llm.txt
    
    # 1. Check if arguments exist, otherwise default to current directory
    set -l search_targets $argv
    if test (count $search_targets) -eq 0
        set search_targets .
    end

    # 2. Define ignores (standard junk folders)
    set -l ignore_dirs node_modules .git .idea .vscode __pycache__ dist build target vendor bin obj .next env venv
    set -l prune_args
    for dir in $ignore_dirs
        set -a prune_args -name $dir -prune -o
    end

    echo "Reading content..."

    # 3. Find files, skip binaries, format output, and write to /tmp/llm.txt
    #    We use 'find' instead of a for-loop so it handles recursion correctly 
    #    if you pass a folder like 'src/'
    find $search_targets $prune_args -type f -print0 | sort -z | xargs -0 -I % sh -c '
        # Binary check (grep -qI checks if text)
        if grep -qI . "%"; then
            echo "================================================================================"
            echo "FILE PATH: %"
            echo "================================================================================"
            cat "%"
            echo ""
            echo ""
        fi
    ' > $output_file

    # 4. Calculate Tokens (Chars / 4)
    #    We read the file size of the temp file
    set -l char_count (wc -c < $output_file | string trim)
    set -l token_est (math "round($char_count / 4)")

    # 5. Handle Clipboard
    #    Checks if your requested 'clip-file' exists, otherwise tries standard tools
    if functions -q clip-file
        clip-file $output_file
        echo "✅ Copied to clipboard using 'clip-file'"
    else if type -q pbcopy
        cat $output_file | pbcopy
        echo "✅ Copied to clipboard (pbcopy)"
    else if type -q wl-copy
        cat $output_file | wl-copy
        echo "✅ Copied to clipboard (wl-copy)"
    else if type -q xclip
        cat $output_file | xclip -selection clipboard
        echo "✅ Copied to clipboard (xclip)"
    else
        echo "⚠️  'clip-file' not found. File saved to $output_file"
    end

    # 6. Print Stats
    set_color cyan
    echo "Files saved to: $output_file"
    echo "Approx Tokens:  ~$token_est"
    set_color normal
end


