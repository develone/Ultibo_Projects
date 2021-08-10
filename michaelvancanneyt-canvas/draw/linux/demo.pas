{$mode objfpc}{$h+}
program demo;
 
uses classes, sysutils,
     FPImage, FPCanvas, FPImgCanv,
     FPWritePNG;
 
var canvas : TFPcustomCanvas;
    image : TFPCustomImage;
    writer : TFPCustomImageWriter;
    { 
      Colors range from 0 to 65535 in each primary color. 
      They can also show as hexideciaml:
      $FFFF = 65535, $0000 = 0 
    }
    passionRed: TFPColor = (Red: 65535; Green: 0; Blue: 0; Alpha: 65535);
begin
  image := TFPMemoryImage.Create (100,100);
  Canvas := TFPImageCanvas.Create (image);
  Writer := TFPWriterPNG.Create;

  { Set the pen styles }
  with canvas do
  begin
    pen.mode    := pmCopy;
    pen.style   := psSolid;
    pen.width   := 1;
    pen.FPColor := passionRed;
  end;

  { Draw a circle }
  canvas.Ellipse (10,10, 90,90);
  
  { Save to file }
  image.SaveToFile ('DrawTest.png', writer);
  
  { Clean up! }
  Canvas.Free;
  image.Free;
  writer.Free;
end.
