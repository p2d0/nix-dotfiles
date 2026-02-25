#!/usr/bin/env bash

# --- Config: Icons ---
ICON_NOW="󱎫"
ICON_NONE="󰃭"

# --- Colors ---
COLOR_TIME="#abb2bf"
COLOR_TITLE="#ffffff"

# 1. Fetch Agenda for Today and Tomorrow
# We use tail -n +2 to skip the header
DATA=$(gcalcli agenda --tsv --nodeclined "$(date +%F)" "$(date -d 'tomorrow' +%F)" 2>/dev/null | tail -n +2)

NOW_TS=$(date +%s)
CURRENT_EVENT_TEXT=""
TOOLTIP="<b><u>Today's Agenda</u></b>\n"
HAS_EVENTS=false

# 2. Parse Data
# Note: We read 5 specific columns, then 'rest' captures everything else.
while IFS=$'\t' read -r sdate stime edate etime title rest; do
    
    # Ignore the separator lines (gcalcli inserts lines that are just date ranges)
    # If the title column is a date or empty, skip it.
    [[ -z "$title" ]] && continue
    [[ "$title" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]] && continue

    HAS_EVENTS=true

    # Handle All-Day events (empty time fields)
    DISPLAY_TIME="$stime"
    [[ -z "$stime" ]] && stime="00:00" && DISPLAY_TIME="All Day"
    [[ -z "$etime" ]] && etime="23:59"

    # Convert to timestamps
    # Using -d with specific format to ensure compatibility
    START_TS=$(date -d "$sdate $stime" +%s 2>/dev/null)
    END_TS=$(date -d "$edate $etime" +%s 2>/dev/null)

    # Building the Tooltip
    TOOLTIP+="\n<span color='$COLOR_TIME'>$DISPLAY_TIME</span>  <span color='$COLOR_TITLE'>$title</span>"

    # THE CHECK: Is it happening now?
    if [[ -n "$START_TS" && -n "$END_TS" ]]; then
        if (( NOW_TS >= START_TS && NOW_TS <= END_TS )); then
            CURRENT_EVENT_TEXT="$ICON_NOW $title"
        fi
    fi
done <<< "$DATA"

# 3. Final Output Logic
if [[ -n "$CURRENT_EVENT_TEXT" ]]; then
    BAR_TEXT="$CURRENT_EVENT_TEXT"
    CLASS="now"
else
    BAR_TEXT="$ICON_NONE No current event"
    CLASS="none"
fi

if [ "$HAS_EVENTS" = false ]; then
    TOOLTIP="No events scheduled for today."
fi

# Escape for Waybar JSON
TOOLTIP_ESC=$(echo -e "$TOOLTIP" | sed ':a;N;$!ba;s/\n/\\n/g' | sed 's/"/\\"/g')

echo "{\"text\": \"$BAR_TEXT\", \"tooltip\": \"$TOOLTIP_ESC\", \"class\": \"$CLASS\"}"
