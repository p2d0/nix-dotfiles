function combine_micro_and_linein
    # pactl load-module module-null-source sink_name=micros sink_properties=device.description="linein and micro mixed"
    # pactl load-module module-loopback source='alsa_input.pci-0000_07_00.4.analog-stereo' sink=micros
    # pactl load-module module-loopback source='alsa_input.usb-C-Media_Electronics_Inc._USB_PnP_Sound_Device-00.mono-fallback' sink=micros
    # pactl load-module module-remap-source master=micros.monitor source_name=micros source_properties=device.description=CombinedSource
    # pactl set-default-sink micros
    # pw-loopback --capture-props="node.name=mic-loopback" --playback-props="media.class=Audio/Sink"
    # pw-loopback --capture-props="node.name=linein-loopback" --playback-props="media.class=Audio/Sink"
    pw-cli create-node adapter '{ factory.name = adapter node.name = "combined-source" media.class = "Audio/Source" }'
    # pw-link 51 <combined-source-id>
end
