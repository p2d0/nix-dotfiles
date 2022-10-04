function encode -a text
    echo $text | openssl aes-256-cbc -a -pbkdf2 -salt -pass pass:(read -s -P "pass> ")
end
