function mkv2exr --description "Convert video to linear EXR frame sequence for Natron"
    if test (count $argv) -lt 1
        echo "Usage: mkv2exr <input_video>"
        return 1
    end

    set -l input $argv[1]

    if not test -f "$input"
        echo "Error: File '$input' does not exist."
        return 1
    end

    # Extract filename without directory path and extension
    set -l dir_name (string replace -r '\.[^.]+$' '' (basename "$input"))

    echo "Creating directory: $dir_name"
    mkdir -p "$dir_name"

    echo "Extracting EXR frames (Linearized for Natron)..."
    ffmpeg -i "$input" \
        -vf "zscale=transfer=linear" \
        -compression zip16 \
        -pix_fmt rgb48le \
        -start_number 1 \
        "$dir_name/frame_%04d.exr"

    echo "Extracting audio track..."
    ffmpeg -i "$input" -vn -c:a pcm_s16le "$dir_name/audio.wav"

    echo "Done! Sequence generated in ./$dir_name/"
end


