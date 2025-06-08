function llm -d "cat the contents of fd copy them to tmp and then cfic"
    fd $argv -X cat > /tmp/llm.txt
    clip-file /tmp/llm.txt
end
