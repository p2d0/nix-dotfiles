#/usr/bin/env nix-shell
#!nix-shell -p sway
if [ $XDG_SESSION_TYPE = "wayland" ]
then
    export SWAYSOCK="/run/user//1000/sway-ipc.1000.1992.sock"
    swaymsg "output * bg /etc/nixos/bg.jpg fill"
else
    feh --bg-fill /etc/nixos/bg_old.png;
    touch $HOME/.xsettingsd
    cat > $HOME/.xsettingsd <<-EOF
    Net/IconThemeName "Obsidian"
    Net/ThemeName "Adwaita-dark"
EOF
    pkill -HUP xsettingsd

fi

# configure-gtk
# gnome_schema=org.gnome.desktop.interface
# gsettings set $gnome_schema gtk-theme 'Adwaita-dark'

