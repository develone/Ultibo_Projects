#!/bin/bash
qemu-system-arm -machine versatilepb -cpu cortex-a8 -kernel kernel.bin -m size=256 \
 -drive file=disk.img,if=sd,format=raw
