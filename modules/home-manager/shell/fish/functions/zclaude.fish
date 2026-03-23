function zclaude --description 'Run Claude CLI via z.ai using ~/.local/bin/claude'
    set -l key_file ~/Dropbox/zai.txt

    # Check if the API key file exists
    if not test -f $key_file
        echo "Error: API key file not found at $key_file"
        return 1
    end

    # Read and trim the key
    set -l zai_key (cat $key_file | string trim)

    # Set environment variables locally for this command
    set -lx ANTHROPIC_BASE_URL "https://api.z.ai/api/anthropic"
    set -lx ANTHROPIC_AUTH_TOKEN "$zai_key"
    # set -lx ANTHROPIC_API_KEY "$zai_key"

    # Run the specific claude binary
    ~/.local/bin/claude $argv
end
