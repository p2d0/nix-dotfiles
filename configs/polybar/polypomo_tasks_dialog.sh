#!/usr/bin/env nix-shell
#!nix-shell -i bash -p sqlite

# Path to the SQLite database
DB_PATH="/etc/nixos/configs/polybar/time.sqlite"

# Fetch tasks from the database
tasks=$(sqlite3 "$DB_PATH" <<EOF
SELECT date, start, stop, title
FROM sessions
WHERE date(date) = date('now')
ORDER BY start;
EOF
)

# Format tasks for zenity
formatted_tasks=""
while IFS= read -r line; do
    date=$(echo "$line" | cut -d'|' -f1)
    start=$(echo "$line" | cut -d'|' -f2)
    stop=$(echo "$line" | cut -d'|' -f3)
    title=$(echo "$line" | cut -d'|' -f4)
    duration=$(date -d "$stop" +%s) - $(date -d "$start" +%s)
    duration=$(date -u -d "@$duration" +%H:%M:%S)
    formatted_tasks+="$date $start - $stop ($duration) - $title\n"
done <<< "$tasks"

# Display tasks in a zenity dialog
zenity --list \
    --title="Today's Tasks" \
    --text="Tasks completed today:" \
    --column="Details" \
    --width=500 \
    --height=400 \
    $formatted_tasks