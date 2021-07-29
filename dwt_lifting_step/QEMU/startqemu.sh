#!/bin/bash
qemu-system-arm -machine versatilepb -cpu cortex-a8 -kernel kernel.bin \
-net user,hostfwd=tcp::5023-:23,hostfwd=udp::5069-:69,hostfwd=tcp::6050-:5050 -net nic \
 -drive file=disk.img,if=sd,format=raw