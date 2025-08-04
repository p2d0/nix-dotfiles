function cva
    if test (count $argv) -ne 2
        echo "Usage: cva <video_file> <audio_file>"
        return 1
    end

    combine_video_audio $argv[1] $argv[2] ../$argv[1]
    rm $argv[1]
    rm $argv[2]
end
