#!/bin/bash
qemu-system-arm -machine raspi2b -cpu cortex-a8 -kernel kernel7.qimg \
-netdev user,id=net0,hostfwd=tcp::5080-:80,hostfwd=tcp::5023-:23,hostfwd=tcp::8050-:5050,hostfwd=udp::5069-:69,hostfwd=tcp::6050-:5050 -device usb-net,netdev=net0 \
 -drive file=disk.img,if=sd,format=raw --device usb-host,vendorid=0x2e8a,productid=0x000a
