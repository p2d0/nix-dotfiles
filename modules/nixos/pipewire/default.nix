{ config, lib, pkgs, ... }:

{
  # hardware.pulseaudio.enable = false;
  # hardware.pulseaudio.support32Bit = false;
  #     environment.etc = {
  #         "pipewire/pipewire.conf".text = ''
  # context.modules = [
  # {   name = libpipewire-module-combine-stream
  #     args = {
  #         combine.mode = sink
  #         node.name = "my_combined_sink"
  #         node.description = "My Combined Sink"
  #         combine.props = {
  #             audio.position = [ FL FR ]
  #                            }
  #         stream.rules = [
  #             {
  #                 matches = [
  #                     {
  #                         media.class = "Audio/Sink"
  #                     }
  #                 ]
  #                 actions = {
  #                     create-stream = {
  #                     }
  #                           }
  #             }
  #                         ]
  #            }
  # }
  #          ]
  # '';
  #     };
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    extraConfig.pipewire = {

      # context.modules = [
      # {   name = "libpipewire-module-filter-chain"
      #     args = {
      #         node.description =  "Noise Canceling source"
      #         media.name =  "Noise Canceling source"
      #         filter.graph = {
      #             nodes = [
      #                 {
      #                     type = ladspa
      #                     name = rnnoise
      #                     plugin = "${pkgs.unstable.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so"
      #                     label = noise_suppressor_stereo
      #                     control = {
      #                         "VAD Threshold (%)" 50.0
      #                             }
      #                 }
      #             ]
      #                       }
      #         capture.props = {
      #             target.object = "alsa_input.usb-C-Media_Electronics_Inc._USB_PnP_Sound_Device-00.mono-fallback"
      #             node.name =  "capture.rnnoise_source"
      #             node.latency = "256/48000"
      #             node.passive = true
      #                        }
      #         playback.props = {
      #             node.name =  "rnnoise_source"
      #             node.latency = "256/48000"
      #             media.class = Audio/Source
      #                         }
      #            }
      # }
      # ];
      "10-noise" = {
        "context.modules" = [
          {
            name = "libpipewire-module-filter-chain";
            args = {
              "node.description" = "Noise Canceling source";
              "media.name" = "Noise Canceling source";
              "filter.graph" = {
                nodes = [
                  {
                    type = "ladspa";
                    name = "rnnoise";
                    plugin = "${pkgs.unstable.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so";
                    label = "noise_suppressor_stereo";
                    control = {
                      "VAD Threshold (%)" = 50.0;
                    };
                  }
                ];
              };
              "capture.props" = {
                "target.object" = "alsa_input.usb-C-Media_Electronics_Inc._USB_PnP_Sound_Device-00.mono-fallback";
                "node.name" = "capture.rnnoise_source";
                "node.latency" = "256/48000";
                "node.passive" = true;
              };
              "playback.props" = {
                "node.name" = "rnnoise_source";
                "node.latency" = "256/48000";
                "media.class" = "Audio/Source";
              };
            };
          }
        ];
      };
    };
    # extraConfig.pipewire = {
    #     "10-combine-sinks" = {
    #         "context.modules" = {
    #             name = "libpipewire-module-combine-stream";
    #             args = {
    #                 combine.mode = "sink";
    #                 node.name = "my_combined_sink";
    #                 node.description = "My Combined Sink";
    #                 combine.props = {
    #                     audio.position = [ "FL" "FR" ];
    #                 };
    #                 stream.rules = [
    #                     {
    #                         matches = [
    #                             {
    #                                 media.class = "Audio/Sink";
    #                             }
    #                         ];
    #                         actions = {
    #                             create-stream = {
    #                             };
    #                         };
    #                     }
    #                 ];
    #             };
    #         };
    #     };
    # };
    # wireplumber.extraLuaConfig.bluetooth."11-bluetooth-policy" =
    #     ''
    #         bluetooth_policy.policy["media-role.use-headset-profile"] = false
    #     ''
    # config.pipewire-pulse = {
    #   "context.modules" = [
    #     {
    #       name = "libpipewire-module-bluetooth-policy";
    #       args = {
    #         "auto_switch" = false;
    #       };
    #       # flags = [ "ifexists" "nofail" ];
    #     }
    #   ];
    # };
    # config.pipewire = {
    #   "context.properties" = {
    #     "default.clock.rate" = 48000;
    #     "default.clock.quantum" = 256;
    #     "default.clock-min-quantum" = 256;
    #   };
    # };
    # config.pipewire-pulse = {
    #   "context.modules" = [
    #     {
    #       name = "libpipewire-module-rtkit";
    #       args = {
    #         "nice.level" = -15;
    #         "rt.prio" = 88;
    #         "rt.time.soft" = 200000;
    #         "rt.time.hard" = 200000;
    #       };
    #       flags = [ "ifexists" "nofail" ];
    #     }
    #     { name = "libpipewire-module-protocol-native"; }
    #     { name = "libpipewire-module-client-node"; }
    #     { name = "libpipewire-module-adapter"; }
    #     { name = "libpipewire-module-metadata"; }
    #     {
    #       name = "libpipewire-module-protocol-pulse";
    #       args = {
    #         "pulse.min.req" = "256/48000";
    #         "pulse.default.req" = "256/48000";
    #         "pulse.max.req" = "256/48000";
    #         "pulse.min.quantum" = "256/48000";
    #         "pulse.max.quantum" = "256/48000";
    #         "server.address" = [ "unix:native" ];
    #       };
    #     }
    #   ];
    #   "stream.properties" = {
    #     "node.latency" = "256/48000";
    #     "resample.quality" = 1;
    #   };
    # };
  };
}
