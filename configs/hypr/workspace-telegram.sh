#!/usr/bin/env bash

WORKSPACE="11"
FIREFOX_URL="https://ug.kyrgyzstan.kg/pomotask/"

# Get current window addresses before launching anything
BEFORE_CLIENTS=$(hyprctl clients -j | jq -r '[.[] | .address] | sort | join("\n")')

# Switch to workspace 11 first
hyprctl dispatch workspace "$WORKSPACE"
sleep 0.3

# Get addresses again now that we're on workspace 11
BEFORE_CLIENTS=$(hyprctl clients -j | jq -r '[.[] | .address] | sort | join("\n")')

# Launch Firefox using firefox-pwa
firefox-pwa "$FIREFOX_URL" &
echo "Launching Firefox PWA..."

# Launch Telegram
Telegram &
echo "Launching Telegram..."

# Wait for new windows to appear
MAX_RETRIES=20
i=0
FIREFOX_ADDR=""
TELEGRAM_ADDR=""

while [ $i -lt $MAX_RETRIES ]; do
    CURRENT_CLIENTS=$(hyprctl clients -j)
    
    # Find new window addresses
    CURRENT_ADDRS=$(echo "$CURRENT_CLIENTS" | jq -r '[.[] | .address] | sort | join("\n")')
    NEW_ADDRS=$(comm -13 <(echo "$BEFORE_CLIENTS") <(echo "$CURRENT_ADDRS"))
    
    if [[ -z "$FIREFOX_ADDR" ]]; then
        while IFS= read -r addr; do
            if echo "$CURRENT_CLIENTS" | jq -r ".[] | select(.address == \"$addr\") | .class" | grep -q "firefox"; then
                FIREFOX_ADDR="$addr"
                echo "Found new Firefox PWA window: $FIREFOX_ADDR"
            fi
        done <<< "$NEW_ADDRS"
    fi
    
    if [[ -z "$TELEGRAM_ADDR" ]]; then
        while IFS= read -r addr; do
            if echo "$CURRENT_CLIENTS" | jq -r ".[] | select(.address == \"$addr\") | .class" | grep -q "org.telegram.desktop"; then
                TELEGRAM_ADDR="$addr"
                echo "Found new Telegram window: $TELEGRAM_ADDR"
            fi
        done <<< "$NEW_ADDRS"
    fi
    
    # Break if both found
    if [[ -n "$FIREFOX_ADDR" ]] && [[ -n "$TELEGRAM_ADDR" ]]; then
        break
    fi
    
    sleep 0.5
    ((i++))
done

if [[ -z "$FIREFOX_ADDR" ]]; then
    echo "Timeout waiting for new Firefox PWA window"
    exit 1
fi

if [[ -z "$TELEGRAM_ADDR" ]]; then
    echo "Timeout waiting for new Telegram window"
    exit 1
fi

# Small delay to let windows settle
sleep 0.5

# Ensure we're on workspace 11
hyprctl dispatch workspace "$WORKSPACE"
sleep 0.2

# Make Telegram the master window (will be on the right since we want Firefox on left as 20%)
hyprctl dispatch focuswindow "address:$TELEGRAM_ADDR"
sleep 0.1
hyprctl dispatch layoutmsg swapwithmaster master

# Set master size factor to exactly 0.8 (80% for Telegram as master, Firefox gets 20%)
# Actually, let's make Firefox the master but at 20% size
# hyprctl dispatch focuswindow "address:$FIREFOX_ADDR"
# sleep 0.1
# hyprctl dispatch layoutmsg swapwithmaster master

# Set master size factor to exactly 0.2 (20% for Firefox)
hyprctl dispatch layoutmsg mfact exact 0.2

echo "Windows positioned successfully. Firefox PWA is 20% on the left, Telegram 80% on the right."
exit 0
