#!/usr/bin/env fish

function empipe
    read -z piped
    echo $piped > /tmp/temp
    em /tmp/temp
end
