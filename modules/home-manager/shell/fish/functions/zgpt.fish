function zgpt --description 'Run Shell GPT via z.ai with glm-4.7 model'
    set -l key_file ~/Dropbox/zai.txt

    # 1. Check if the API key file exists
    if not test -f $key_file
        echo "Error: API key file not found at $key_file"
        return 1
    end

    # 2. Read and trim the key
    set -l zai_key (cat $key_file | string trim)

    # 3. Set environment variables locally for this execution
    # Base URL and Auth
    set -lx OPENAI_API_KEY "$zai_key"
    set -lx OPENAI_API_BASE "https://api.z.ai/api/coding/paas/v4/"
    set -lx API_BASE_URL "https://api.z.ai/api/coding/paas/v4/"
    
    # Model and Behavior
    set -lx DEFAULT_MODEL "glm-4.7"
    set -lx DEFAULT_COLOR "magenta"
    set -lx REQUEST_TIMEOUT 60
    
    # Caching and Functions
    set -lx CHAT_CACHE_PATH "/tmp/shell_gpt/chat_cache"
    set -lx CACHE_PATH "/tmp/shell_gpt/cache"
    set -lx OPENAI_USE_FUNCTIONS true
    set -lx SHOW_FUNCTIONS_OUTPUT false
    
    # 4. Execute sgpt with passed arguments
    sgpt $argv
end


