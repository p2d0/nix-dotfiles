#!/usr/bin/env bash

# --- Config: Colors ---
# Customize these to match your Waybar theme
p_color="#ff7a93" # Doable Color
r_color="#a6e3a1" # Repeatable Color

get_waybar_data() {
    # 1. Fetch JSON from Emacs
    local raw_json
    raw_json=$(emacsclient --eval '(my-eww-get-todos-json)' 2>/dev/null | jq -r 2>/dev/null)

    # 2. Handle Emacs Offline/Nil
    if [[ -z "$raw_json" || "$raw_json" == "nil" || "$raw_json" == "null" ]]; then
        echo '{"text":"󰄭 Offline","tooltip":"Emacs server not responding","class":"offline"}'
        return
    fi

    # 3. Process JSON - Priority: Repeatable -> Doable
    echo "$raw_json" | jq -r --arg pc "$p_color" --arg rc "$r_color" '
        # Main text: Check Repeatable list first, then Doable
        (.Repeatable[0] // .Doable[0] // "All Done!") as $curr |
        
        # Tooltip construction: Repeatable section at the top
        "<b><u>Daily Tasks</u></b>\n\n" +
        (if (.Repeatable | length) > 0 then 
            "<span color=\"" + $rc + "\"><b>Repeatable:</b></span>\n" + ([.Repeatable[] | " • " + .] | join("\n")) + "\n\n" 
        else "" end) +
        (if (.Doable | length) > 0 then 
            "<span color=\"" + $pc + "\"><b>Doable:</b></span>\n" + ([.Doable[] | " • " + .] | join("\n")) 
        else "" end)
        as $tooltip |

        # Output Waybar-compatible JSON
        {
            text: (" " + $curr),
            tooltip: $tooltip,
            class: (if $curr == "All Done!" then "completed" else "pending" end)
        }
    ' -c
}

# --- Initialization ---

# Get the daily file path from Emacs and strip the surrounding quotes
FILES_TO_WATCH=$(emacsclient --eval '(my-get-todays-daily-path)' 2>/dev/null | sed 's/^"\(.*\)"$/\1/')

# Print initial data for Waybar startup
get_waybar_data

# Wait logic: if the file path is invalid, wait for Emacs to provide it
if [[ -z "$FILES_TO_WATCH" || "$FILES_TO_WATCH" == "nil" ]]; then
    while [[ -z "$FILES_TO_WATCH" || "$FILES_TO_WATCH" == "nil" ]]; do
        sleep 5
        FILES_TO_WATCH=$(emacsclient --eval '(my-get-todays-daily-path)' 2>/dev/null | sed 's/^"\(.*\)"$/\1/')
    done
fi

# --- Watcher Loop ---

while true; do
    # Block until file change detected
    # If inotifywait errors (e.g. file deleted), sleep 1 to prevent high CPU loop
    inotifywait -q -e close_write,moved_to,modify "$FILES_TO_WATCH" > /dev/null 2>&1 || sleep 1
    
    # stdbuf -oL forces Line Buffering so Waybar receives updates instantly
    get_waybar_data
done
