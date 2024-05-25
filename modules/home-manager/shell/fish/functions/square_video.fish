function square_video -a video
    # Define the output file name by appending _square to the input video name (without extension)
    set output_file (basename $video .mp4)_square.mp4

    # Use ffmpeg to crop the video to a square
    # ffmpeg -i $video -vf "crop=min(iw\,ih):min(iw\,ih):(iw-min(iw\,ih))/2:(ih-min(iw\,ih))/2" $output_file
    # ffmpeg -i $video -vf "crop=min(iw\,ih):min(iw\,ih):(iw-min(iw\,ih))/2:(ih-min(iw\,ih))/2" -c:v libx264 -crf 23 -preset medium -c:a aac -b:a 128k $output_file
    # ffmpeg -i $video -vf "crop=639:639:(iw-639)/2:(ih-639)/2" $output_file
    # ffmpeg -i $video -vf "crop='min(639,iw)':'min(639,ih)':(iw-min(639,iw))/2:(ih-min(639,ih))/2" $output_file
    # ffmpeg -i $video -vf "crop='min(639,iw)':'min(639,ih)':(iw-min(639,iw))/2:(ih-min(639,ih))/2" $output_file

    # Get video dimensions
    set dims (ffprobe -v error -select_streams v:0 -show_entries stream=width,height -of csv=s=x:p=0 $video)
    set width (echo $dims | cut -d 'x' -f 1)
    set height (echo $dims | cut -d 'x' -f 2)

    # Calculate the crop size
    set crop_size (math "min(639, $width, $height)")

    # Crop the video to a square and output in MPEG4 format
    ffmpeg -i $video -vf "crop=$crop_size:$crop_size:((iw-$crop_size)/2):((ih-$crop_size)/2)" $output_file




end
