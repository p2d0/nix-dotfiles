#/usr/bin/env nix-shell
#!nix-shell -p sway feh
source /etc/set-environment;

if [ "$XDG_SESSION_TYPE" = "wayland" ];
then
    export SWAYSOCK="/run/user/$(id -u)/sway-ipc.$(id -u).$(pidof sway).sock"
    swaymsg "output * bg /etc/nixos/bg_old.png fill"
else
    if [ "$WORK_MODE" = "1" ]; then
        feh --bg-fill /etc/nixos/work-bg.jpg;
    else
        feh --bg-fill /etc/nixos/bg_old.png;
    fi
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

