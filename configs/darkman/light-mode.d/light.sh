#/usr/bin/env nix-shell
#!nix-shell -p feh
export SWAYSOCK="/run/user//1000/sway-ipc.1000.1992.sock"

# touch $HOME/.config/gtk-3.0/settings.ini;
# cat > $HOME/.config/gtk-3.0/settings.ini <<-EOF
# [Settings]
# gtk-icon-theme-name=Obsidian
# gtk-theme-name=Breeze
# EOF
# feh --bg-fill /etc/nixos/light.jpg;
swaymsg "output * bg /etc/nixos/light.jpg fill"
# EOF

# touch $HOME/.xsettingsd
# cat > $HOME/.xsettingsd <<-EOF
# Net/IconThemeName "Obsidian"
# Net/ThemeName "Adwaita"
# EOF
# configure-gtk
# gnome_schema=org.gnome.desktop.interface
# gsettings set $gnome_schema gtk-theme 'Adwaita'
# gsettings set $gnome_schema icon-theme 'Obsidian'

# pkill -HUP xsettingsd

# emacsclient -e '(toggle-day-night-theme :light)'
