#!/usr/bin/env sh

# A script that watches Org files and calls emacsclient to fetch todos on change.

# Ensure inotifywait is installed
if ! command -v inotifywait &> /dev/null; then
    echo "Error: inotify-tools is not installed. Please install it." >&2
    exit 1
fi

# Function to get fresh data from Emacs
get_data() {
    # The --eval argument prints the result of the Lisp form to stdout.
    # Eww will read this line.
    emacsclient --eval '(my-eww-get-todays-json)' | jq -r
}

# --- Main Logic ---

# 1. Get the list of files to watch directly from our running Emacs instance.
# We use `eval` here because the output of the elisp function is a valid shell string.
FILES_TO_WATCH=$(eval emacsclient --eval "'(my-get-todays-daily-path)'")
FILES_TO_WATCH="${FILES_TO_WATCH//\"}"

if [ -z "$FILES_TO_WATCH" ]; then
    echo "Error: Could not get file list from Emacs. Is the server running?" >&2
    exit 1
fi

# 2. Print the initial state immediately so Eww has data on startup.
get_data

# 3. Start the infinite loop to watch for changes.
while true; do
    # inotifywait will block here until one of the watched files is
    # modified, written to, created, or moved.
    inotifywait -q -e close_write,moved_to,create,modify $FILES_TO_WATCH

    # Once a change is detected, get the new data and print it.
    get_data
done
