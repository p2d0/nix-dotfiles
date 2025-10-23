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
  config = mkIf cfg.enable
    {
      home.activation.linkFirefoxChrome = lib.hm.dag.entryAfter ["writeBoundary"] ''
    # This script will symlink the Firefox chrome directory from your NixOS config.

    # Step 1: Define the source directory in your configuration.
    source_dir="/etc/nixos/configs/firefox/chrome"

    # Step 2: Find the default Firefox profile directory.
    # We only act if exactly one '*.default' directory is found.
    # This is safer than 'head -n 1' if multiple profiles exist.
    shopt -s nullglob # Ensure the array is empty if no match is found
    profiles=("$HOME"/.mozilla/firefox/*.default)
    shopt -u nullglob

    # Step 3: Proceed only if the source exists and exactly one profile was found.
    if [[ -d "$source_dir" && ''${#profiles[@]} -eq 1 ]]; then
      profile_dir="''${profiles[0]}"
      target_path="$profile_dir/chrome"

      verboseEcho "Found Firefox profile: $profile_dir"
      verboseEcho "Linking '$source_dir' to '$target_path'"

      # Step 4: Perform the file system operations using the 'run' command.
      # This respects the DRY_RUN variable.

      # First, remove any existing directory or symlink at the target path.
      # This prevents `ln` from creating a link *inside* an existing directory.
      run rm -rf "$target_path"

      # Then, create the new symbolic link. $VERBOSE_ARG will add the --verbose
      # flag to ln if home-manager switch is run with --verbose.
      run ln -s $VERBOSE_ARG "$source_dir" "$target_path"

    else
      # Provide helpful diagnostic messages in verbose mode if conditions aren't met.
      if ! [[ -d "$source_dir" ]]; then
        verboseEcho "Skipping Firefox chrome link: source directory not found at '$source_dir'"
      fi
      if [[ ''${#profiles[@]} -eq 0 ]]; then
        verboseEcho "Skipping Firefox chrome link: no '*.default' profile directory found."
      fi
      if [[ ''${#profiles[@]} -gt 1 ]]; then
        verboseEcho "Skipping Firefox chrome link: multiple '*.default' profiles found, cannot determine which one to use."
      fi
    fi
  '';
    };
}
