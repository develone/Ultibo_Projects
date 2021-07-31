#!/bin/bash
qemu-system-arm -machine versatilepb -cpu cortex-a8 -kernel kernel.bin \
-net user,hostfwd=tcp::9080-:80,hostfwd=tcp::9023-:23,hostfwd=udp::9069-:69,hostfwd=tcp::9050-:5050 -net nic \
 -drive file=disk.img,if=sd,format=raw
