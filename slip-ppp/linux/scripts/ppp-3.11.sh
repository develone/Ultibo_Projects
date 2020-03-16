#!/bin/bash
#mypi3-11
sudo stty -F /dev/ttyUSB0 raw
sudo stty -F /dev/ttyUSB0 -a

sudo pppd /dev/ttyUSB0 1000000 10.0.5.2:10.0.5.1 noauth local debug dump defaultroute nocrtscts persist maxfail 0 holdoff 1
