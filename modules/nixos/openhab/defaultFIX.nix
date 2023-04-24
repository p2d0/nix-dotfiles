{ config, lib, pkgs, ... }:
let
  username = "openhabsrvtry";
  pathTmpOpenhab = "/var/lib/try.openhab";
in
{
    imports = [ ];

    environment = with pkgs;
    {
      systemPackages =
      [
        mc
        jre
        gawk
        procps
      ];
    };

    users.users = lib.singleton
    {
        name = username;
        description = "OpenHAB Server User (Try)";
    };

    systemd.services.openhabtry =
    {
      description = "OpenHAB Server Try";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];   ## TODO: is it?

      environment =
      {
          JAVA_HOME = "${pkgs.jre}"; # where to look for java.exe
      };
      path = [ "${pkgs.gawk}/bin" "${pkgs.procps}/bin"];

      serviceConfig =
      {
        ExecStart  = "${pathTmpOpenhab}/runtime/bin/karaf server";
        ExecStop   = "${pathTmpOpenhab}/runtime/bin/karaf stop";

        Restart = "always";
        RestartSec = "10";
        TimeoutStopSec = "30";  # Assume might be executing shutdown scripts

        User = username;
      };
    };

    # Otherwise it's UTC
    time.timeZone = "Europe/Moscow";
}
