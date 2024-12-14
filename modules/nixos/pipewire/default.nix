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
