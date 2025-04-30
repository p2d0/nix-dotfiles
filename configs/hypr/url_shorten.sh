#!/usr/bin/env bash

# Get clipboard content
url=$(wl-paste)

# Basic URL detection regex
if [[ $url =~ ^https?://.+ ]]; then
    # Shorten URL using is.gd
    shortened=$(curl -s "https://is.gd/create.php?format=simple&url=${url}")

    if [[ $shortened =~ ^https?://.+ ]]; then
        # Copy the shortened URL back to clipboard
        wl-copy "$shortened"
        echo "✅ URL shortened and copied to clipboard:"
        echo "$shortened"
    else
        echo "❌ Failed to shorten URL."
        exit 1
    fi
else
    echo "📎 Clipboard does not contain a valid URL."
    exit 1
fih
