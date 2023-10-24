#!/usr/bin/env bash
# requires wg-quick to be in sudoers file

wg-generate(){
    cd $HOME;
    wgcf register --accept-tos;
    wgcf generate;
}

wg-start(){
    cd $HOME;
    sudo wg-quick up wgcf-profile.conf
}

wg-stop(){
    cd $HOME;
    sudo wg-quick down wgcf-profile.conf
}

is_off(){
    wg_status=$(wg show 2>&1);
    if [ -z  "$wg_status" ]; then
        return 0;
    fi
    return 1;
}

show_status(){
    if is_off; then
        echo ' OFF'
    else
        echo  ' ON'
    fi
}

wg-toggle(){
    if is_off; then
        if [ ! -f "$HOME/wgcf-profile.conf" ]; then
            wg-generate;
        fi
        wg-start
        until ping -c 2 google.com; do
            wg-stop;
            wg-generate;
            wg-start;
        done
    else
        wg-stop
    fi
}

case "$1" in
    --status)
        if pgrep -u $UID -x wgcf > /dev/null; then
            echo "Starting..."
        else
            show_status
        fi
        ;;
    *)
        wg-toggle
        ;;
esac
