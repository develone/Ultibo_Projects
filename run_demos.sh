#!/bin/bash

RPI2="192.168.1.185"
RPI3="192.168.1.200"
#RPI2="192.168.0.102"
#RPI3="192.168.0.101" 
if [ $2 = "demo" ]; then
	cd Demo/RPi2
	if [ -e kernel7.img ]; then
		if [ $1 = "185" ]; then
			echo "tftping UltiboDemoRPi2_TFTP kernel7.img" 
			echo "to ultibo system " $RPI2
			tftp $RPI2 < cmdstftp
		else
			echo "tftping UltiboDemoRPi2_TFTP kernel7.img"
			echo "to ultibo system " $RPI3
			tftp $RPI3 < cmdstftp
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
				 
				echo "No camera  ultibo sytem " $RPI2
		
			else
				echo "tftping RaspiVid_TFTP kernel7.img"
				echo "to ultibo system " $RPI3
				tftp $RPI3 < cmdstftp
			fi
		else	
			echo "no kernel7.img"
		fi	
		cd ../../
	else
			if [ $2 = "jpeg" ]; then
				cd jpeg2000/RPi2
				if [ -e kernel7.img ]; then
					if [ $1 = "185" ]; then
						echo "tftping DWT_LIFT_RPi2 kernel7.img"
						echo "to ultibo system " $RPI2
						tftp $RPI2 < cmdstftp
		
					else
						echo "tftping DWT_LIFT_RPi2 kernel7.img"
						echo "to ultibo system " $RPI3
						tftp $RPI3 < cmdstftp
					fi
			else	
				echo "no kernel7.img"
			fi	
			cd ../../
		else
			if [ $2 = "playvideo" ]; then
				cd HelloPi/HelloVideo_TFTP/RPi2/
				if [ -e kernel7.img ]; then
					if [ $1 = "185" ]; then
						echo "tftping HelloVideo_TFTP kernel7.img"
						echo "to ultibo system " $RPI2
						tftp $RPI2 < cmdstftp
		
					else
						echo "tftping HelloVideo_TFTP kernel7.img"
						echo "to ultibo system " $RPI3
						tftp $RPI3 < cmdstftp
					fi
				else	
					echo "no kernel7.img"
				fi	
				cd ../../../
		else
			if [ $2 = "servo" ]; then
				cd Ultibo_PWM_TFTP/RPi2
				if [ -e kernel7.img ]; then
					if [ $1 = "185" ]; then
						echo "tftping PWM_TFTP_SERVO kernel7.img"
						echo "to ultibo system " $RPI2
						tftp $RPI2 < cmdstftp
		
					else
							 
						echo "no servo " $RPI3
							 
					fi
				else	
					echo "no kernel7.img"
				fi	
				cd ../../
			else
				if [ $2 = "gps" ]; then
					cd gps/RPi2
					if [ -e kernel7.img ]; then
						if [ $1 = "185" ]; then
							echo "tftping gps kernel7.img"
							echo "to ultibo system " $RPI2
							tftp $RPI2 < cmdstftp
		
					else
							 
						echo "no gps " $RPI3
							 
					fi
				else	
					echo "no kernel7.img"
				fi	
				cd ../../
			else
			
				if [ $2 = "RemoteLed" ]; then
					cd RemoteLed/RPi2
					if [ -e kernel7.img ]; then
						if [ $1 = "185" ]; then
							echo "tftping RemoteLed kernel7.img"
							echo "to ultibo system " $RPI2
							tftp $RPI2 < cmdstftp
		
						else
							echo "tftping RemoteLed kernel7.img"
							echo "to ultibo system " $RPI3
							tftp $RPI3 < cmdstftp	 	 
						fi
					else	
						echo "no kernel7.img"
				fi	
				cd ../../
			else
			
				if [ $2 = "OpenGLES" ]; then
					cd OpenGLES/HelloGLES/RPi2
					if [ -e kernel7.img ]; then
						if [ $1 = "185" ]; then
							echo "tftping HelloGLES  kernel7.img"
							echo "to ultibo system " $RPI2
							tftp $RPI2 < cmdstftp
		
						else
							echo "tftping HelloGLES  kernel7.img"
							echo "to ultibo system " $RPI3
							tftp $RPI3 < cmdstftp	 	 
						fi
					else	
						echo "no kernel7.img"
				fi	
				cd ../../../
			fi
			fi
			fi	
		fi
fi
fi
fi
fi

