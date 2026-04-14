function gclaude --description 'Run Claude CLI via z.ai using ~/.local/bin/claude'
    # Set environment variables locally for this command
    set -lx ANTHROPIC_BASE_URL "http://127.0.0.1:8045"
    set -lx ANTHROPIC_AUTH_TOKEN "sk-antigravity"
    # set -lx ANTHROPIC_API_KEY "$zai_key"

    # Run the specific claude binary
    ~/.local/bin/claude $argv
end


