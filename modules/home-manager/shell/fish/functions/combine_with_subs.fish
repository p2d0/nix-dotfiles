function combine_with_subs -a vid -a sub
    set fontsize 35
    ffmpeg -i $vid -vf "subtitles=$sub:force_style='FontName=Roboto,FontSize=$fontsize'" -c:v libx264 -x264-params crf=22 -preset fast -profile:v high "out_$vid"
end
