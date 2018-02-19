# Ultibo_Projects
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
