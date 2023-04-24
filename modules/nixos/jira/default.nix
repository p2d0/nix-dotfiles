{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.jira;
in {
  options.modules.jira = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable {
    services.postgresql = {
      enable = true;
      authentication = pkgs.lib.mkOverride 10 ''
      local all all trust
      host all all 127.0.0.1/32 trust
      host all all ::1/128 trust
    '';
      initialScript = pkgs.writeText "backend-initScript" ''
      CREATE USER jiradbadmin WITH PASSWORD 'password';
      CREATE DATABASE jiradb WITH ENCODING 'UNICODE' LC_COLLATE 'C' LC_CTYPE 'C' TEMPLATE template0;
      GRANT ALL PRIVILEGES ON DATABASE jiradb TO jiradbadmin;
    '';  };
    services.jira = {
      enable = true;
      jrePackage = pkgs.openjdk8;
      catalinaOptions = ["-javaagent:/mnt/md127/Downloads/atlassian-agent/target/atlassian-agent-jar-with-dependencies.jar"];
      package = pkgs.atlassian-jira.overrideAttrs(oldAttrs: rec {
        postInstall = ''
      rm -f $out/atlassian-jira/WEB-INF/lib/atlassian-extras-3.4.6.jar
      cd $out/atlassian-jira/WEB-INF/lib
      ${pkgs.wget} https://omwtfyb.ir/atlassian-extras-3.4.6.jar'';
      });
      # KEY: AAAB+g0ODAoPeJyNU12PojAUfedXkOzjBmzRwY+kySriyIo6DrCT9a3iVTrD17bFGffXLwhmPjRmE15o7zn3nHNvv3lFqg5zrmKkImOA+wNkqoFvqQYyDGXPAdIoy3PgustCSAX4xxwWNAFiLedz+9Fyhq5icaCSZemYSiAVUEMdDSPlBmQMIuQsr1AkSGOWMAlbNa4B6uaoRlLmYtBq/Y1YDDrLlDllqYSUpiHYbznjx6Zbr6+hbvkpz4zTs0p7y2rqhevMHd8eK4si2QBf7gIBXBANn8Xd4Mp5ti1CqVc/msh28pVy0C+IbtTSULIDEMkL+JTlx/Mb8FIVtaB0zevSJp5fZePKnKF4xeY9xlOJfaBxcRoG2dFYNPRfiZZ8T1Mm6roq6TJo3G/ruIN1bJg6xr1BDyGsWFkqS7F2GX5MhJ7oz/RAt6y8+rFPyjM9zJK6xUUsjdgpFRGZW8iarKxJ27v/zhLjPnl9Sqcrx/Lcdb4wZv31ygmmeBYf//xOWiNv7XaXuwhPneDhpZV1Ealb/GdqnqS8clr7b8bsjInrjD17obnY7PTv7rq4Y5oIf9qaa4vqAT8AL+Gj0ZOtIfcn1oKlMdEc358pL3A8DwObCHVRr93G117N5T4+FDyMqICvb+Yj+DSxnDPRmC7lkysWmiGdlI+G/j+VCElqMCwCFGxrnjz1G0V5MDwiLbn3gPiP6BUqAhRxlZk+6MN8sZsehGcEYqlhbQxMyg==X02o0
    };

  };
}
