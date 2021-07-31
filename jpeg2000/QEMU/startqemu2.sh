#!/bin/bash
qemu-system-arm -machine versatilepb -cpu cortex-a8 -kernel kernel.bin \
-net user,hostfwd=tcp::8080-:80,hostfwd=tcp::8023-:23,hostfwd=udp::8069-:69,hostfwd=tcp::8050-:5050 -net nic \
 -drive file=disk.img,if=sd,format=raw
