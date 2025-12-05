# function llm -d "cat the contents of fd copy them to tmp and then cfic"
#     fd $argv -X cat > /tmp/llm.txt
#     clip-file /tmp/llm.txt
# end

function llm -d "cat the contents of fd copy them to tmp and then cfic"
     cat $argv > /tmp/llm.txt
     clip-file /tmp/llm.txt
end
