{$mode objfpc} {$h+}

{export PATH=/home/devel/ultibo/core/fpc/bin:$PATH
fpc -B -Tlinux -Parm @/home/devel/ultibo/core/fpc/bin/fpc.cfg -O2 ImgConv.pas
Free Pascal Compiler version 3.1.1 [2021/07/22] for arm
Copyright (c) 1993-2015 by Florian Klaempfl and others
Target OS: Linux for ARMHF
Compiling ImgConv.pas
Linking ImgConv
47 lines compiled, 0.9 sec}

program ImgConv;

uses
	FPWriteXPM, FPWritePNG, FPWriteBMP,
	FPReadXPM, FPReadPNG, FPReadBMP, fpreadjpeg, fpreadtga,
	fpreadpnm, FPImage, sysutils;

var
	img : TFPMemoryImage;
	ReadFile, WriteFile : string;

begin
	if paramcount =2 then
		begin
		ReadFile := paramstr(1);
		WriteFile := paramstr(2);
		end
 	else
		begin
		Writeln('Usage: ./ImgConv infile outfile');
		Halt(1);
		end;
	if CompareText(ReadFile,WriteFile)=0 then
		begin
			Writeln('Input file cannot be the same as output file');
  		Halt(1);
			end;
	Img:=TFPMemoryImage.create(0,0);
	try
		Img.LoadFromFile(ReadFile);
		Img.SaveToFile(WriteFile);
	Finally
		Img.Free;
	end;
	end.
	
