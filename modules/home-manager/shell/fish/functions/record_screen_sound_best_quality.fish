function record_screen_sound_best_quality
    set -l monitor (hyprctl monitors -j | jq -r '.[] | select(.focused == true) | .name')
    test -z "$monitor"; and set monitor "screen"
    set -l filename ~/Videos/(date +%Y-%m-%d_%H-%M-%S).mp4
    gpu-screen-recorder \
        -w $monitor \
        -f 60 \
        -k hevc \
        -q ultra \
        -bm qp \
        -tune performance \
        -fm cfr \
        -cr limited \
        -c mp4 \
        -a default_output \
        -ab 320 \
        -o $filename
    clip-file $filename
end
