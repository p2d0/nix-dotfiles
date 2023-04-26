#/usr/bin/env nix-shell
#!nix-shell -p feh
if [ $XDG_SESSION_TYPE = "wayland" ]
then
    export SWAYSOCK="/run/user/$(id -u)/sway-ipc.$(id -u).$(pidof sway).sock"
    swaymsg "output * bg /etc/nixos/light.jpg fill"
else
    feh --bg-fill /etc/nixos/light.jpg;

    touch $HOME/.xsettingsd
    cat > $HOME/.xsettingsd <<-EOF
    Net/IconThemeName "Obsidian"
    Net/ThemeName "Adwaita"
EOF
    pkill -HUP xsettingsd
fi

# touch $HOME/.config/gtk-3.0/settings.ini;
# cat > $HOME/.config/gtk-3.0/settings.ini <<-EOF
# [Settings]
# gtk-icon-theme-name=Obsidian
# gtk-theme-name=Breeze
# EOF
# EOF

# configure-gtk
# gnome_schema=org.gnome.desktop.interface
# gsettings set $gnome_schema gtk-theme 'Adwaita'
# gsettings set $gnome_schema icon-theme 'Obsidian'


# emacsclient -e '(toggle-day-night-theme :light)'
