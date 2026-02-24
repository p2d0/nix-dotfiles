#!/run/current-system/sw/bin/bash

# --- Colors (Edit these as you like) ---
p_color="#ff7a93" # Pending / Doable
r_color="#a6e3a1" # Repeatable

get_waybar_data() {
    # 1. Fetch JSON from Emacs
    local raw_json=$(emacsclient --eval '(my-eww-get-todos-json)' 2>/dev/null | jq -r)

    if [[ -z "$raw_json" || "$raw_json" == "nil" ]]; then
        echo '{"text": "Emacs Offline", "tooltip": "Start Emacs server", "class": "error"}'
        return
    fi

    # 2. Process your specific JSON structure:
    # {"Repeatable": ["..."], "Doable": ["..."]}
    echo "$raw_json" | jq -r --arg pc "$p_color" --arg rc "$r_color" '
        # Define which task shows up on the bar (Priority: Doable first, then Repeatable)
        (.Doable[0] // .Repeatable[0] // "All Done!") as $curr |

        # Build the Tooltip with Pango Markup
        "<b><u>Daily Tasks</u></b>\n\n" +
        (if (.Doable | length) > 0 then 
            "<span color=\"" + $pc + "\"><b>Doable:</b></span>\n" + ([.Doable[] | " • " + .] | join("\n")) + "\n\n" 
        else "" end) +
        (if (.Repeatable | length) > 0 then 
            "<span color=\"" + $rc + "\"><b>Repeatable:</b></span>\n" + ([.Repeatable[] | " • " + .] | join("\n")) 
        else "" end)
        as $tooltip |

        # Output Waybar JSON format
        {
            text: (" " + $curr),
            tooltip: $tooltip,
            class: (if $curr == "All Done!" then "completed" else "pending" end)
        }
    ' -c
}

# --- Main Logic (Watcher) ---

# Get the file path from Emacs
FILES_TO_WATCH=$(eval emacsclient --eval "'(my-get-todays-daily-path)'" 2>/dev/null)
FILES_TO_WATCH="${FILES_TO_WATCH//\"}"

if [ -z "$FILES_TO_WATCH" ]; then
    echo '{"text": "No Org File", "tooltip": "Waiting for Emacs..."}'
    exit 1
fi

# Initial output
get_waybar_data

# Watch for changes and update Waybar instantly
while true; do
    # This blocks until the org file is saved
    inotifywait -q -e close_write,moved_to,create,modify "$FILES_TO_WATCH" > /dev/null
    get_waybar_data
done
