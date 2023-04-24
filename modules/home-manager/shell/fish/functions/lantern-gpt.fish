function lantern-gpt
    pkill sslocal;
    sslocal -c /mnt/md127/shadowsocks/sslong.conf --local-addr localhost:8091 -d
    export http_proxy=socks5://127.0.0.1:8091
    export https_proxy=socks5://127.0.0.1:8091
    chat-gpt &
    disown %1
end
