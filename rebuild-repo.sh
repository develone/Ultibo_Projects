#!/bin/bash
#this script is run with Ultibo_Projects/rebuild-repo.sh
#the --recursive is needed to fetch the freertos repo.
#the pico-sdk is needed with tinyUSB

cd ~/

rm -rf rp2040-freertos-project

git clone  --recursive git@github.com:develone/rp2040-freertos-project

cd rp2040-freertos-project


cd ../

mkdir build
