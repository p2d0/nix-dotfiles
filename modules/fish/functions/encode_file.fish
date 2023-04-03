function encode_file -a file
    gpg --output $file.enc --symmetric --no-symkey-cache --cipher-algo AES256 $file
end
