function decode -a text
    echo $text | openssl aes-256-cbc -pbkdf2 -d -a -pass pass:(read -s -P "pass> ")
end
