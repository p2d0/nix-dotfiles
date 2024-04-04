{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.ilzabot;
in {
  options.modules.ilzabot = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    systemd.user.services.ilzabot =
      let dir = "/mnt/md127/ilzabot";
      in
        {
          enable = true;
          wantedBy = ["default.target"];
          environment = {
            https_proxy = "http://localhost:8092";
            LD_LIBRARY_PATH = lib.makeLibraryPath [
              pkgs.stdenv.cc.cc
              pkgs.glib
              pkgs.libglvnd
            ];
          };
          path = ["/run/current-system/sw"];
          serviceConfig = {
            WorkingDirectory="${dir}";
            ExecStart = "${dir}/.venv/bin/python ${dir}/iLzabot.py";
          };
        };

    # virtualisation.oci-containers = {
    #   backend = "docker";
    #   containers = {
    #     ilzabot = {
    #       autoStart = true;
    #       image = "ilza";
    #     };
    #   };
    # };
  };
}
