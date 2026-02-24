#!/usr/bin/env bash

# --- Config ---
p_color="#ff7a93" 
r_color="#a6e3a1"

get_waybar_data() {
    # Fetch from Emacs
    local raw_json
    raw_json=$(emacsclient --eval '(my-eww-get-todos-json)' 2>/dev/null | jq -r 2>/dev/null)

    # Handle Emacs being closed or returning nil
    if [[ -z "$raw_json" || "$raw_json" == "nil" ]]; then
        echo '{"text":"󰄭","tooltip":"Emacs Offline","class":"offline"}'
        return
    fi

    # Process JSON
    echo "$raw_json" | jq -r --arg pc "$p_color" --arg rc "$r_color" '
        (.Doable[0] // .Repeatable[0] // "All Done!") as $curr |
        
        "<b><u>Daily Tasks</u></b>\n\n" +
        (if (.Doable | length) > 0 then 
            "<span color=\"" + $pc + "\"><b>Doable:</b></span>\n" + ([.Doable[] | " • " + .] | join("\n")) + "\n\n" 
        else "" end) +
        (if (.Repeatable | length) > 0 then 
            "<span color=\"" + $rc + "\"><b>Repeatable:</b></span>\n" + ([.Repeatable[] | " • " + .] | join("\n")) 
        else "" end)
        as $tooltip |

        {
            text: (" " + $curr),
            tooltip: $tooltip,
            class: (if $curr == "All Done!" then "completed" else "pending" end)
        }
    ' -c
}

# 1. Get the file path
# Use sed to strip quotes reliably
FILES_TO_WATCH=$(emacsclient --eval '(my-get-todays-daily-path)' 2>/dev/null | sed 's/^"\(.*\)"$/\1/')

# 2. Initial Run
get_waybar_data

# 3. The Loop
if [[ -z "$FILES_TO_WATCH" || ! -f "$FILES_TO_WATCH" ]]; then
    # If file doesn't exist yet, don't crash. Just poll every 10s until it does.
    while [[ ! -f "$FILES_TO_WATCH" ]]; do
        sleep 10
        FILES_TO_WATCH=$(emacsclient --eval '(my-get-todays-daily-path)' 2>/dev/null | sed 's/^"\(.*\)"$/\1/')
    done
fi

# Use stdbuf to ensure Waybar sees the output immediately
while true; do
    # Watch the file. If inotifywait fails, the sleep 1 acts as a safety backup.
    inotifywait -q -e close_write,moved_to,modify "$FILES_TO_WATCH" > /dev/null 2>&1 || sleep 1
    
    # Use stdbuf to force line buffering
    stdbuf -oL get_waybar_data
done
