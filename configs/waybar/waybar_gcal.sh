#!/usr/bin/env bash

# --- Config ---
ICON_CAL="󰃭"
ICON_NOW="󱎫"
ICON_NEXT="󰔟"

COLOR_NOW="#51afef"
COLOR_NEXT="#9b59b6"

# 1. Fetch Agenda
# We use tail -n +2 to skip the TSV header row
DATA=$(gcalcli agenda --tsv --nodeclined "$(date +%F)" "$(date -d 'tomorrow' +%F)" 2>/dev/null | tail -n +2)

if [[ -z "$DATA" ]]; then
    echo "{\"text\": \"$ICON_CAL No Events\", \"tooltip\": \"Calendar is clear!\", \"class\": \"empty\"}"
    exit 0
fi

NOW_TS=$(date +%s)
CURRENT_EVENT=""
NEXT_EVENT=""
TOOLTIP="<b><u>Today's Agenda</u></b>\n"

while IFS=$'\t' read -r sdate stime edate etime title; do
    # Skip rows that don't have a title (usually day headers in TSV)
    [[ -z "$title" ]] && continue
    # Skip the header row if tail failed to catch it
    [[ "$sdate" == "start_date" ]] && continue

    # Handle All-Day events (where time might be empty)
    [[ -z "$stime" ]] && stime="00:00"
    [[ -z "$etime" ]] && etime="23:59"

    # Convert times to timestamps
    START_TS=$(date -d "$sdate $stime" +%s 2>/dev/null)
    END_TS=$(date -d "$edate $etime" +%s 2>/dev/null)
    
    # Skip if date conversion failed
    [[ $? -ne 0 ]] && continue

    # Format line for tooltip
    TOOLTIP+="\n<span color='#abb2bf'>$stime</span> <b>$title</b>"

    # Check if event is happening NOW
    if (( NOW_TS >= START_TS && NOW_TS <= END_TS )); then
        # If multiple events now, it will show the last one found
        CURRENT_EVENT="$ICON_NOW $title"
    fi

    # Check for next upcoming event (if nothing is happening now)
    if [[ -z "$CURRENT_EVENT" && -z "$NEXT_EVENT" && "$START_TS" -gt "$NOW_TS" ]]; then
        NEXT_EVENT="$ICON_NEXT ($stime) $title"
    fi
done <<< "$DATA"

# 3. Determine Output
if [[ -n "$CURRENT_EVENT" ]]; then
    TEXT="$CURRENT_EVENT"
    CLASS="now"
elif [[ -n "$NEXT_EVENT" ]]; then
    TEXT="$NEXT_EVENT"
    CLASS="next"
else
    TEXT="$ICON_CAL All Done"
    CLASS="done"
fi

# Final JSON Assembly (Clean multi-line for Tooltip)
TOOLTIP_ESC=$(echo -e "$TOOLTIP" | sed ':a;N;$!ba;s/\n/\\n/g' | sed 's/"/\\"/g')

echo "{\"text\": \"$TEXT\", \"tooltip\": \"$TOOLTIP_ESC\", \"class\": \"$CLASS\"}"
