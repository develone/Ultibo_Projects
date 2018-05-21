import serial
file = open("sixzeros.bin","wb")
file.write("\x00\x00\x00\x00\x00\x00")
file.close

