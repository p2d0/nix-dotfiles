{ config, lib, pkgs, ... }:

with lib;
let cfg = config.modules.firefox;
in {
  options.modules.firefox = {
    enable = mkOption {
      type = types.bool;
      default = false;
      example = "";
      description = ''
      '';
    };
  };
  config = mkIf cfg.enable (lib.my.withHome
    (args:{
home.activation.linkFirefoxChrome = args.lib.hm.dag.entryAfter ["writeBoundary"] ''
    # Step 1: Define the source directory in your configuration.
    source_dir="/etc/nixos/configs/firefox/chrome"

    # Step 2: Define possible base directories for Firefox profiles
    # We check both standard and XDG-style paths
    base_dirs=("$HOME/.mozilla/firefox" "$HOME/.config/mozilla/firefox")
    
    profiles=()
    shopt -s nullglob
    for base in "''${base_dirs[@]}"; do
        # Append any .default directories found in this base to our array
        profiles+=("$base"/*.default)
    done
    shopt -u nullglob

    # Step 3: Proceed only if the source exists and exactly one profile was found.
    if [[ -d "$source_dir" && ''${#profiles[@]} -eq 1 ]]; then
      profile_dir="''${profiles[0]}"
      target_path="$profile_dir/chrome"

      verboseEcho "Found Firefox profile: $profile_dir"
      verboseEcho "Linking '$source_dir' to '$target_path'"

      # Step 4: Perform the file system operations.
      # Remove any existing directory or symlink at the target path.
      run rm -rf "$target_path"

      # Create the new symbolic link.
      run ln -s $VERBOSE_ARG "$source_dir" "$target_path"

    else
      # Diagnostic messages
      if ! [[ -d "$source_dir" ]]; then
        verboseEcho "Skipping Firefox chrome link: source directory not found at '$source_dir'"
      fi
      if [[ ''${#profiles[@]} -eq 0 ]]; then
        verboseEcho "Skipping Firefox chrome link: no profile directory found in standard locations."
      fi
      if [[ ''${#profiles[@]} -gt 1 ]]; then
        verboseEcho "Skipping Firefox chrome link: found ''${#profiles[@]} profiles, cannot determine which one to use."
      fi
    fi
'';
    })
    {
      environment.systemPackages = [
        pkgs.firefoxpwa
      ];

      programs.firefox = {
        enable = true;
        package = pkgs.unstable.firefox;
        nativeMessagingHosts.packages = [ pkgs.firefoxpwa ];
      };
    });
}

