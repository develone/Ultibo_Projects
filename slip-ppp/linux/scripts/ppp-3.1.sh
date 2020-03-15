#!/bin/bash
#mypi3-1
sudo stty -F /dev/ttyUSB0 raw
sudo stty -F /dev/ttyUSB0 -a

sudo pppd /dev/ttyUSB0 230400 10.0.5.1:10.0.5.2 noauth local debug dump defaultroute nocrtscts persist maxfail 0 holdoff 1
