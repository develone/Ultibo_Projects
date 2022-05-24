#!/bin/bash
#this script is run with Ultibo_Projects/rebuild-repo.sh
#the --recurse-submodule is needed to fetch the freertos repo.

cd ~/

rm -rf rp2040-freertos-project

git clone  --recurse-submodules git@github.com:develone/rp2040-freertos-project

cd rp2040-freertos-project

git clone git@github.com:develone/pico-sdk.git

git clone https://github.com/raspberrypi/pico-sdk

cd pico-sdk

git submodule update --init

cd ../

mkdir build
