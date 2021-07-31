#!/bin/bash
qemu-system-arm -machine versatilepb -cpu cortex-a8 -kernel kernel.bin \
-net user,hostfwd=tcp::11080-:80,hostfwd=tcp::11023-:23,hostfwd=udp::11069-:69,hostfwd=tcp::11050-:5050 -net nic \
 -drive file=disk.img,if=sd,format=raw
