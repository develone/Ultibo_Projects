program fontdraw;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FPimage, FPImgCanv, ftfont, FPWritePNG, FPCanvas;

procedure TestFPImgFont;
var
  Img: TFPMemoryImage;
  Writer: TFPWriterPNG;
  ms: TMemoryStream;
  ImgCanvas: TFPImageCanvas;
  fs: TFileStream;
  AFont: TFreeTypeFont;
begin
  Img:=nil;
  ImgCanvas:=nil;
  Writer:=nil;
  ms:=nil;
  fs:=nil;
  AFont:=nil;
  try
    // initialize free type font manager
    ftfont.InitEngine;
    FontMgr.SearchPath:='/usr/share/fonts/truetype/dejavu/';
    //FontMgr.SearchPath:='/usr/share/fonts/truetype/ttf-dejavu/';
    AFont:=TFreeTypeFont.Create;

    // create an image of width 200, height 100
    Img:=TFPMemoryImage.Create(200,100);
    Img.UsePalette:=false;
    // create the canvas with the drawing operations
    ImgCanvas:=TFPImageCanvas.create(Img);

    // paint white background
    ImgCanvas.Brush.FPColor:=colWhite;
    ImgCanvas.Brush.Style:=bsSolid;
    ImgCanvas.Rectangle(0,0,Img.Width,Img.Height);

    // paint text
    ImgCanvas.Font:=AFont;
    ImgCanvas.Font.Name:='DejaVuSans';
    ImgCanvas.Font.Size:=20;
    ImgCanvas.TextOut(10,30,'Test');

    // write image as png to memory stream
    Writer:=TFPWriterPNG.create;
    ms:=TMemoryStream.Create;
    writer.ImageWrite(ms,Img);
    // write memory stream to file
    ms.Position:=0;
    fs:=TFileStream.Create('testfont.png',fmCreate);
    fs.CopyFrom(ms,ms.Size);
  finally
    AFont.Free;
    ms.Free;
    Writer.Free;
    ImgCanvas.Free;
    Img.Free;
    fs.Free;
  end;
end;

begin
  TestFPImgFont;
end.
