#!/bin/bash
 
if [ $2 = "demo" ]; then
	cd Demo/RPi2
	if [ -e kernel7.img ]; then
		if [ $1 = "185" ]; then
			echo "tftping UltiboDemoRPi2_TFTP kernel7.img"
			echo "to ultibo sytem 192.168.1.185"
			tftp 192.168.1.185 < cmdstftp
		else
			echo "tftping UltiboDemoRPi2_TFTP kernel7.img"
			echo "to ultibo sytem 192.168.1.200"
			tftp 192.168.1.200 < cmdstftp
		fi
	else	
		echo "no kernel7.img"
	fi	
	cd ../../
else 
	if [ $2 = "video" ]; then
		cd RaspiVid_TFTP/RPi2/
		if [ -e kernel7.img ]; then
			if [ $1 = "185" ]; then
				echo "tftping UltiboDemoRPi2_TFTP kernel7.img"
				echo "No camera  ultibo sytem 192.168.1.185"
		
			else
				echo "tftping RaspiVid_TFTP kernel7.img"
				echo "to ultibo sytem 192.168.1.200"
				tftp 192.168.1.200 < cmdstftp
			fi
	else	
		echo "no kernel7.img"
	fi	
	cd ../../
fi
fi

