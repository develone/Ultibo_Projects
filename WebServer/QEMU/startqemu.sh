#!/bin/bash
qemu-system-arm -machine versatilepb -cpu cortex-a8 -kernel kernel.bin \
-net user,hostfwd=tcp::5080-:80,hostfwd=tcp::5023-:23 -net nic \
 -drive file=disk.img,if=sd,format=raw
