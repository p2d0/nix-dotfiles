function h265 -a input -a output -d "Convert file to x265 codec"
    if test (count $argv) -lt 2
        echo "Takes two arguments input and output"
        return;
    end
    ffmpeg -i "$input" -c:v libx265 -c:a copy "$output";;
end
