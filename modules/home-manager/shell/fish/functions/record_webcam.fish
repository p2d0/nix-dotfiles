function record_webcam
    # - Audio
    # f alsa -ac 1 -ar 48000 -i hw:3 -acodec aac
    # # -f alsa -i hw:2
    # -f alsa -ac 1 -i hw:3
    # With sound
    # ffmpeg -f pulse -ac 1 -i alsa_input.usb-C-Media_Electronics_Inc._USB_PnP_Sound_Device-00.pro-input-0  -framerate 30 -f v4l2 -video_size 1280x720 -i /dev/video0  -vf 'format=nv12,hwupload' -b:v 1M -vaapi_device /dev/dri/renderD128 -c:v hevc_vaapi -filter:a "volume=4"   (date "+/home/andrew/Videos/%Y-%m-%d").mp4
    # No sound
    ffmpeg -framerate 30 -f v4l2 -video_size 1280x720 -i /dev/video0  -vf 'format=nv12,hwupload' -b:v 1M -vaapi_device /dev/dri/renderD128 -c:v hevc_vaapi  (date "+/home/andrew/Videos/%Y-%m-%d").mp4
end
