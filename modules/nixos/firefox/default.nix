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
      programs.firefoxpwa = {
        enable = true;
        profiles."00000000000000000000000000".sites = {
          "01KHERYP04RMDFAT6ATRJKE17V" = {
            name = "ChatGPT";
            url = "https://chat.openai.com/";
            manifestUrl = "data:application/manifest+json;base64,eyJzdGFydF91cmwiOiJodHRwczovL2NoYXRncHQuY29tLyIsIm5hbWUiOiJDaGF0R1BUIiwiZGVzY3JpcHRpb24iOiJDaGF0R1BUIGlzIHlvdXIgQUkgY2hhdGJvdCBmb3IgZXZlcnlkYXkgdXNlLiBDaGF0IHdpdGggdGhlIG1vc3QgYWR2YW5jZWQgQUkgdG8gZXhwbG9yZSBpZGVhcywgc29sdmUgcHJvYmxlbXMsIGFuZCBsZWFybiBmYXN0ZXIuIiwiaWNvbnMiOlt7InNyYyI6Imh0dHBzOi8vY2hhdGdwdC5jb20vZmF2aWNvbi5pY28iLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiJhbnkifSx7InNyYyI6Imh0dHBzOi8vY2hhdGdwdC5jb20vY2RuL2Fzc2V0cy9mYXZpY29uLWVleDE3ZTllLmljbyIsInB1cnBvc2UiOiJhbnkiLCJzaXplcyI6IjQ4eDQ4In0seyJzcmMiOiJodHRwczovL2NoYXRncHQuY29tL2Nkbi9hc3NldHMvZmF2aWNvbi1sNG5xMDhoZC5zdmciLCJ0eXBlIjoiaW1hZ2Uvc3ZnK3htbCIsInB1cnBvc2UiOiJhbnkiLCJzaXplcyI6ImFueSJ9LHsic3JjIjoiaHR0cHM6Ly9jaGF0Z3B0LmNvbS9jZG4vYXNzZXRzL2Zhdmljb24tbDRucTA4aGQuc3ZnIiwidHlwZSI6ImltYWdlL3N2Zyt4bWwiLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiJhbnkifSx7InNyYyI6Imh0dHBzOi8vY2hhdGdwdC5jb20vY2RuL2Fzc2V0cy9mYXZpY29uLTQ4eDQ4LTB0bTc2dWwwLndlYnAiLCJ0eXBlIjoiaW1hZ2UvcG5nIiwicHVycG9zZSI6ImFueSIsInNpemVzIjoiNDh4NDgifSx7InNyYyI6Imh0dHBzOi8vY2hhdGdwdC5jb20vY2RuL2Fzc2V0cy9mYXZpY29uLTE4MHgxODAta3FxNW5kbTEud2VicCIsInB1cnBvc2UiOiJhbnkiLCJzaXplcyI6IjE4MHgxODAifSx7InNyYyI6Imh0dHBzOi8vY2hhdGdwdC5jb20vY2RuL2Fzc2V0cy9mYXZpY29uLTQ4eDQ4LWdsbnBlcG0wLndlYnAiLCJ0eXBlIjoiaW1hZ2UvcG5nIiwicHVycG9zZSI6ImFueSIsInNpemVzIjoiNDh4NDgifSx7InNyYyI6Imh0dHBzOi8vY2hhdGdwdC5jb20vY2RuL2Fzc2V0cy9mYXZpY29uLTE4MHgxODAtb2Q0NWVjaTYud2VicCIsInB1cnBvc2UiOiJhbnkiLCJzaXplcyI6IjE4MHgxODAifV19";
          };
          "01KHESNMWKH22H49Y8FQG6FZGD" = {
            name = "grok";
            url = "https://grok.com/";
            manifestUrl = "https://grok.com/manifest.webmanifest";
          };
          "01KHGBETHAKQNPND0N6Q2KYS23" = {
            name = "Google AI Studio";
            url = "https://aistudio.google.com/prompts/new_chat";
            manifestUrl = "data:application/manifest+json;base64,eyJzdGFydF91cmwiOiJodHRwczovL2Fpc3R1ZGlvLmdvb2dsZS5jb20vcHJvbXB0cy9uZXdfY2hhdCIsIm5hbWUiOiJHb29nbGUgQUkgU3R1ZGlvIiwiZGVzY3JpcHRpb24iOiJUaGUgZmFzdGVzdCBwYXRoIGZyb20gcHJvbXB0IHRvIHByb2R1Y3Rpb24gd2l0aCBHZW1pbmkiLCJpY29ucyI6W3sic3JjIjoiaHR0cHM6Ly93d3cuZ3N0YXRpYy5jb20vYWlzdHVkaW8vYWlfc3R1ZGlvX2Zhdmljb25fMl8zMngzMi5wbmciLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiIzMngzMiJ9LHsic3JjIjoiaHR0cHM6Ly93d3cuZ3N0YXRpYy5jb20vYWlzdHVkaW8vYWlfc3R1ZGlvX2Zhdmljb25fMl8zMngzMi5wbmciLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiIzMngzMiJ9LHsic3JjIjoiaHR0cHM6Ly93d3cuZ3N0YXRpYy5jb20vYWlzdHVkaW8vYWlfc3R1ZGlvX2Zhdmljb25fMl82NHg2NC5wbmciLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiI2NHg2NCJ9LHsic3JjIjoiaHR0cHM6Ly93d3cuZ3N0YXRpYy5jb20vYWlzdHVkaW8vYWlfc3R1ZGlvX2Zhdmljb25fMl82NHg2NC5wbmciLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiI2NHg2NCJ9LHsic3JjIjoiaHR0cHM6Ly93d3cuZ3N0YXRpYy5jb20vYWlzdHVkaW8vYWlfc3R1ZGlvX2Zhdmljb25fMl85Nng5Ni5wbmciLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiI5Nng5NiJ9LHsic3JjIjoiaHR0cHM6Ly93d3cuZ3N0YXRpYy5jb20vYWlzdHVkaW8vYWlfc3R1ZGlvX2Zhdmljb25fMl85Nng5Ni5wbmciLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiI5Nng5NiJ9LHsic3JjIjoiaHR0cHM6Ly93d3cuZ3N0YXRpYy5jb20vYWlzdHVkaW8vYWlfc3R1ZGlvX2Zhdmljb25fMl8xMjh4MTI4LnBuZyIsInB1cnBvc2UiOiJhbnkiLCJzaXplcyI6IjEyOHgxMjgifSx7InNyYyI6Imh0dHBzOi8vd3d3LmdzdGF0aWMuY29tL2Fpc3R1ZGlvL2FpX3N0dWRpb19mYXZpY29uXzJfMTI4eDEyOC5wbmciLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiIxMjh4MTI4In0seyJzcmMiOiJodHRwczovL3d3dy5nc3RhdGljLmNvbS9haXN0dWRpby9haV9zdHVkaW9fZmF2aWNvbl8yXzE4MHgxODAucG5nIiwicHVycG9zZSI6ImFueSIsInNpemVzIjoiMTgweDE4MCJ9LHsic3JjIjoiaHR0cHM6Ly93d3cuZ3N0YXRpYy5jb20vYWlzdHVkaW8vYWlfc3R1ZGlvX2Zhdmljb25fMl8xODB4MTgwLnBuZyIsInB1cnBvc2UiOiJhbnkiLCJzaXplcyI6IjE4MHgxODAifSx7InNyYyI6Imh0dHBzOi8vd3d3LmdzdGF0aWMuY29tL2Fpc3R1ZGlvL2FpX3N0dWRpb19mYXZpY29uXzJfMjU2eDI1Ni5wbmciLCJwdXJwb3NlIjoiYW55Iiwic2l6ZXMiOiIyNTZ4MjU2In0seyJzcmMiOiJodHRwczovL3d3dy5nc3RhdGljLmNvbS9haXN0dWRpby9haV9zdHVkaW9fZmF2aWNvbl8yXzI1NngyNTYucG5nIiwicHVycG9zZSI6ImFueSIsInNpemVzIjoiMjU2eDI1NiJ9XX0=";
          };
          "01KHGBRFWVNS2GDV2EKBTES6CB" = {
            name = "DeepSeek - Into the Unknown";
            url = "https://chat.deepseek.com/";
            manifestUrl = "data:application/manifest+json;base64,eyJzdGFydF91cmwiOiJodHRwczovL2NoYXQuZGVlcHNlZWsuY29tLyIsIm5hbWUiOiJEZWVwU2VlayAtIEludG8gdGhlIFVua25vd24iLCJkZXNjcmlwdGlvbiI6IkNoYXQgd2l0aCBEZWVwU2VlayBBSSDigJMgeW91ciBpbnRlbGxpZ2VudCBhc3Npc3RhbnQgZm9yIGNvZGluZywgY29udGVudCBjcmVhdGlvbiwgZmlsZSByZWFkaW5nLCBhbmQgbW9yZS4gVXBsb2FkIGRvY3VtZW50cywgZW5nYWdlIGluIGxvbmctY29udGV4dCBjb252ZXJzYXRpb25zLCBhbmQgZ2V0IGV4cGVydCBoZWxwIGluIEFJLCBuYXR1cmFsIGxhbmd1YWdlIHByb2Nlc3NpbmcsIGFuZCBiZXlvbmQuIHwg5rex5bqm5rGC57Si77yIRGVlcFNlZWvvvInliqnlipvnvJbnqIvku6PnoIHlvIDlj5HjgIHliJvmhI/lhpnkvZzjgIHmlofku7blpITnkIbnrYnku7vliqHvvIzmlK/mjIHmlofku7bkuIrkvKDlj4rplb/mlofmnKzlr7nor53vvIzpmo/ml7bkuLrmgqjmj5Dkvpvpq5jmlYjnmoRBSeaUr+aMgeOAgiIsImljb25zIjpbeyJzcmMiOiJodHRwczovL2Nkbi5kZWVwc2Vlay5jb20vY2hhdC9pY29uLnBuZyIsInB1cnBvc2UiOiJhbnkifSx7InNyYyI6Imh0dHBzOi8vY2hhdC5kZWVwc2Vlay5jb20vZmF2aWNvbi5zdmciLCJ0eXBlIjoiaW1hZ2UveC1pY29uIiwicHVycG9zZSI6ImFueSJ9XX0=";
          };
          "01KHGFZJW5A9WBS3CZPFAFKXPJ" = {
            name = "Notion Calendar";
            url = "https://calendar.notion.so/";
            manifestUrl = "data:application/manifest+json;base64,eyJzdGFydF91cmwiOiJodHRwczovL2NhbGVuZGFyLm5vdGlvbi5zby8iLCJuYW1lIjoiRmViIDE1LCAyMDI2IMK3IE5vdGlvbiBDYWxlbmRhciIsImljb25zIjpbeyJzcmMiOiJodHRwczovL2NhbGVuZGFyLm5vdGlvbi5zby9Dcm9uLWZhdmljb24tc3R5bGUyLTMxQDJ4LnBuZyIsInB1cnBvc2UiOiJhbnkifSx7InNyYyI6Imh0dHBzOi8vY2FsZW5kYXIubm90aW9uLnNvL0Nyb24tZmF2aWNvbi1zdHlsZTItMzEuc3ZnIiwicHVycG9zZSI6ImFueSJ9LHsic3JjIjoiaHR0cHM6Ly9jYWxlbmRhci5ub3Rpb24uc28vQ3Jvbi1mYXZpY29uLXN0eWxlMi0xNUAyeC5wbmciLCJwdXJwb3NlIjoiYW55In0seyJzcmMiOiJodHRwczovL2NhbGVuZGFyLm5vdGlvbi5zby9Dcm9uLWZhdmljb24tc3R5bGUyLTE1LnN2ZyIsInB1cnBvc2UiOiJhbnkifV19";
          };
        };
      };
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
        policies = {
          Preferences = {
            "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
          };
        };
      };
    });
}

