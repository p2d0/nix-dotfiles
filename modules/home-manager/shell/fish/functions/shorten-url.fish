function shorten-url --description 'Shorten a URL from Wayland clipboard using tinyurl.com'
    # Check dependencies
    for cmd in wl-paste notify-send curl
        if not command -q $cmd
            notify-send "URL Shortener Error" "$cmd not found. Please install required tools."
            return 1
        end
    end

    # Get URL from clipboard
    set url (wl-paste --no-newline)

    # Validate clipboard content
    if test -z "$url"
        notify-send -u normal "URL Shortener Error" "Clipboard is empty."
        return 1
    end

    # Basic URL validation
    if not string match -qr '^https?://' "$url"
        notify-send -u normal "URL Shortener Error" "Clipboard does not contain a valid URL starting with http:// or https://"
        return 1
    end

    # Use tinyurl.com API equivalent
    set response (curl -s "https://tinyurl.com/api-create.php?" --get --data-urlencode "url=$url")

    # Validate shortened URL format
    if string match -qr '^https?://tinyurl.com/[a-zA-Z0-9]+$' "$response"
        echo -n "$response" | wl-copy
        notify-send -u normal "URL Shortener" "Shortened URL: $response (copied to clipboard)"
    else
        notify-send "URL Shortener Error" "Failed to shorten URL. API response: $response"
        return 1
    end
end
