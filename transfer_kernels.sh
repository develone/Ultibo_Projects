#!/bin/bash
echo $1

RPI2="192.168.1.179"

if [ $1 = '1' ]; then	
	cd HelloTeapot/RPi2
	if [ -e kernel7.img ]; then
		pwd
		ls -la kernel7.img
		tftp $RPI2 < cmdstftp
		cd ../../
	else 
		echo 'no kernel7.img'
	fi
fi

if [ $1 = '2' ]; then	
	cd Demo/RPi2
	if [ -e kernel7.img ]; then
		pwd
		ls -la kernel7.img
		tftp $RPI2 < cmdstftp
		cd ../../
	else 
		echo 'no kernel7.img'
	fi
fi

if [ $1 = '3' ]; then	
	cd jpeg2000/RPi2
	if [ -e kernel7.img ]; then
		pwd
		ls -la kernel7.img
		tftp $RPI2 < cmdstftp
		cd ../../
	else 
		echo 'no kernel7.img'
	fi
fi

if [ $1 = '4' ]; then	
	cd gpsudpserver/RPi2
	if [ -e kernel7.img ]; then
		pwd
		ls -la kernel7.img
		tftp $RPI2 < cmdstftp
		cd ../../
	else 
		echo 'no kernel7.img'
	fi
fi

if [ $1 = '5' ]; then	
	cd OpenGLES/HelloGLES/RPi2
	if [ -e kernel7.img ]; then
		pwd
		ls -la kernel7.img
		tftp $RPI2 < cmdstftp
		cd ../../../
	else 
		echo 'no kernel7.img'
	fi
fi

if [ $1 = '6' ]; then	
	cd wall/RPi2
	if [ -e kernel7.img ]; then
		pwd
		ls -la kernel7.img
		tftp $RPI2 < cmdstftp
		cd ../../
	else 
		echo 'no kernel7.img'
	fi
fi

if [ $1 = '7' ]; then	
	cd HelloVideo/RPi2
	if [ -e kernel7.img ]; then
		pwd
		ls -la kernel7.img
		tftp $RPI2 < cmdstftp
		cd ../../
	else 
		echo 'no kernel7.img'
	fi
fi

if [ $1 = '8' ]; then	
	cd RemoteLed/RPi2
	if [ -e kernel7.img ]; then
		pwd
		ls -la kernel7.img
		tftp $RPI2 < cmdstftp
		cd ../../
	else 
		echo 'no kernel7.img'
	fi
fi
