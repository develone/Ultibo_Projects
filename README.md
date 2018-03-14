# Ultibo_Projects
#RaspiVid_TFTP/RPi2/RaspiVid_TFTP.lpi requires a RPi Camera
# HelloPi/HelloVideo_TFTP/RPi2/HelloVideo_TFTP.lpi
Plays the camera.h264 video that can be created with
 

    The Raspbian RPi3B was created using the zip file that I created with pi-gen.
    https://github.com/develone/pi-gen
    Details to create the zip file with pi-gen are found
    https://github.com/develone/raspbian-pi-gen.
    The version provides the Lazarus IDE (Ultibo Edition)
    https://github.com/develone/raspbian-pi-gen/blob/master/ultibo_raspbian.doc
    Appendix A show the 11 steps required to create the zip file.
    https://github.com/develone/raspbian-pi-gen/blob/master/first_boot.doc
    provides the steps to get the Raspbian RPi3B ready to start developing
    Ultibo project with just a few clicks.
    See https://github.com/develone/raspbian-pi-gen/blob/master/lazarus_demo.doc

git clone https://github.com/develone/Ultibo_Projects.git
JPEG2000 Compression
cd Ultibo_Projects/jpeg2000/RPi2
./libbuild.sh
	Compiles the file dwtlift.c and adds the libopenjp2_e357de.a --> libdwtlift.a
The DWT_LIFT_RPi2.lpi compiles and links libdwtlift.a --> kernel7.img
		DWT_LIFT_RPi2.lpr
		uliftbitmap.pas
		uBufferToC.pas
		uTFTP.pas
tftp xxx.xxx.xxx.xxx < cmdstftp transfers kernel7.img to a Ultibo RPi2B or RPi3B		
		
Files required on the micro sd card
	bootcode.bin
	fixup.dat
	start.elf
	kernel7.img
	MyBitmap.bmp file to be compressed
Creates the file 
	test.j2k

