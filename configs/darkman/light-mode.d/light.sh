#/usr/bin/env nix-shell
#!nix-shell -p feh

# touch $HOME/.config/gtk-3.0/settings.ini;
# cat > $HOME/.config/gtk-3.0/settings.ini <<-EOF
# [Settings]
# gtk-icon-theme-name=Obsidian
# gtk-theme-name=Breeze
# EOF
feh --bg-fill /etc/nixos/light.jpg;

touch $HOME/.xsettingsd
cat > $HOME/.xsettingsd <<-EOF
Net/IconThemeName "Obsidian"
Net/ThemeName "Adwaita"
EOF
pkill -HUP xsettingsd

# emacsclient -e '(toggle-day-night-theme :light)'
