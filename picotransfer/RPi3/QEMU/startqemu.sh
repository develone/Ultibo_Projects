#!/bin/bash
qemu-system-arm -machine raspi2b -cpu cortex-a8 -kernel kernel7.qimg \
-net user,hostfwd=tcp::5080-:80,hostfwd=tcp::5023-:23,hostfwd=tcp::8050-:5050,hostfwd=udp::5069-:69,hostfwd=tcp::6050-:5050 -net nic \
 -drive file=disk.img,if=sd,format=raw -device usb-host,hostbus=1,hostaddr=3
