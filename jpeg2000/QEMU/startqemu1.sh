#!/bin/bash
qemu-system-arm -machine versatilepb -cpu cortex-a8 -kernel kernel.bin \
-net user,hostfwd=tcp::7080-:80,hostfwd=tcp::7023-:23,hostfwd=udp::7069-:69,hostfwd=tcp::7050-:5050 -net nic \
 -drive file=disk.img,if=sd,format=raw
