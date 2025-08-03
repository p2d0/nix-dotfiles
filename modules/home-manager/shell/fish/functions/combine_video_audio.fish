function combine_video_audio
    if test (count $argv) -ne 3
        echo "Usage: combine_video_audio <video_file> <audio_file> <output_file>"
        return 1
    end

    set video_file $argv[1]
    set audio_file $argv[2]
    set output_file $argv[3]

    if not test -f $video_file
        echo "Error: Video file '$video_file' not found"
        return 1
    end

    if not test -f $audio_file
        echo "Error: Audio file '$audio_file' not found"
        return 1
    end

    ffmpeg -i $video_file -i $audio_file -c:v copy -c:a copy -map 0:v:0 -map 1:a:0 -shortest $output_file
    if test $status -eq 0
        echo "Successfully created $output_file"
    else
        echo "Error: Failed to combine video and audio"
        return 1
    end
end
