# function llm -d "cat the contents of fd copy them to tmp and then cfic"
#     fd $argv -X cat > /tmp/llm.txt
#     clip-file /tmp/llm.txt
# end

# function llm -d "cat the contents of fd copy them to tmp and then cfic"
#      cat $argv > /tmp/llm.txt
#      clip-file /tmp/llm.txt
# end

function llm -d "cat the contents of fd copy them to tmp and then cfic"
    for file in $argv
        echo "$file:"
        cat -- "$file"
        echo ""
    end > /tmp/llm.txt
    
    clip-file /tmp/llm.txt
end

