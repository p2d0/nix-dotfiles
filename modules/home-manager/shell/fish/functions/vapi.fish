function vapi -a input -a output -d "Convert video to VAPI codec"
    if test (count $argv) -lt 2
        echo "Takes two arguments input and output"
        return;
    end
    ffmpeg -i $input -c:v libvpx-vp9 -pass 2 -b:v 1000K -threads 8 -speed 1 \
        -tile-columns 6 -frame-parallel 1 -auto-alt-ref 1 -lag-in-frames 25 \
        -c:a libopus -b:a 64k -f webm $output
end
