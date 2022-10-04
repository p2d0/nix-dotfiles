function compare-ls -d "Compare ls before and after some action i.e disconnecting a device"
    ls -1 > /tmp/before.txt;
    read -n 1 -P "Press any key to proceed..." yn
    ls -1 > /tmp/after.txt;
    diff /tmp/before.txt /tmp/after.txt;
end
