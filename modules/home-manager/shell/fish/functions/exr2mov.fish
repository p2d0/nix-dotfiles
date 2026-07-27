function exr2mov --description "Convert standard EXR sequence directly to 60fps ProRes 4444 MOV with alpha"
    if test (count $argv) -lt 1
        echo "Usage: exr2mov <output_name[.mov]> [glob_pattern]"
        return 1
    end

    # Strip .mov extension if provided, then enforce .mov extension
    set -l raw_name (string replace -r '\.mov$' '' $argv[1])
    set -l output "$raw_name.mov"

    # Default to frame_*.exr pattern
    set -l pattern "frame_*.exr"
    if test (count $argv) -ge 2
        set pattern $argv[2]
    end

    echo "Encoding matching '$pattern' to ProRes 4444 ($output)..."
    ffmpeg -apply_trc iec61966_2_1 \
           -framerate 60 \
           -pattern_type glob \
           -i "$pattern" \
           -c:v prores_ks \
           -profile:v 4 \
           -pix_fmt yuva444p10le \
           -y "$output"

    echo "Done! Output saved to $output"
end
