#/usr/bin/env nix-shell
#!nix-shell -p feh

feh --bg-fill /etc/nixos/bg_old.png;

touch $HOME/.xsettingsd
cat > $HOME/.xsettingsd <<-EOF
Net/IconThemeName "Obsidian"
Net/ThemeName "Adwaita-dark"
EOF
pkill -HUP xsettingsd
