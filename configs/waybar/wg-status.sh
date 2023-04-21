#!/usr/bin/env zsh
# requires wg-quick to be in sudoers file
alias wg-generate="cd $HOME;wgcf register --accept-tos; wgcf generate;"
alias wg-start="cd $HOME;sudo wg-quick up wgcf-profile.conf"
alias wg-stop="cd $HOME;sudo wg-quick down wgcf-profile.conf"

is_off(){
    wg_status=$(wg show 2>&1);
    if [ -z  "$wg_status" ]; then
        return 0;
    fi
    return 1;
}

show_status(){
    if is_off; then
        echo '{"text":" OFF"}'
    else
        echo  '{"text":" ON","class":"active"}'
    fi
}

wg-toggle(){
    if is_off; then
        wg-start
        if ping google.com; then
        else
            wg-stop;
            wg-generate;
            wg-start;
        fi
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
