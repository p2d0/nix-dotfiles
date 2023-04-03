function decode_file -a file
    gpg --output (string replace '.enc' '' $file) --decrypt $file
end
