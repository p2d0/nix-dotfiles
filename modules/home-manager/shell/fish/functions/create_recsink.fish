function create_recsink
    set sink (pactl get-default-sink)
    pactl load-module module-null-sink sink_name=recsink
    pactl load-module module-loopback source=recsink.monitor sink=$sink
end
