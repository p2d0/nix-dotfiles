
function flash_with_config -a config
    make clean
    make $config
    CROSS_COMPILE=/mnt/md126/sunxi/gcc/bin/arm-linux-gnueabihf- make -j7
    sunxi-fel -v uboot u-boot-sunxi-with-spl.bin
end
