program imgprocess1;

{$mode objfpc}{$H+}
{$linklib cvtutils}

{ Raspberry Pi 3 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }
{_$define UseFile}
uses
  QEMUVersatilePB,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  SysUtils,
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  Classes,
  Ultibo,
  FPImage, FPWriteXPM, FPWritePNG, FPWriteBMP, FPReadXPM, FPReadPNG, FPReadBMP, fpreadjpeg, fpwritejpeg, fpreadtga, fpwritetga,
  fpreadpnm, fpwritepnm,
  uTFTP,
  Winsock2,
  { needed to use ultibo-tftp  }
  { needed for telnet }
  Shell,
  ShellFilesystem,
  ShellUpdate,
  RemoteShell,
  { needed for telnet }
  Logging,
  Syscalls,     {Include the Syscalls unit to provide C library support}
  Crypto,
  APICrypto

  { Add additional units here };

function asciiValueToBinary(x0:LongWord):LongWord; cdecl; external 'libcvtutils' name 'asciiValueToBinary';
type

CBC = record
  {0123456789abcdef0123456789abcdef}
  StrKeyAsc:String[32];
  {0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef}
  StrKeyHex:String[80];
  {0123456789abcdef}
  Strplaintext:array [1..1024] of String[16];
  StrIV:array [1..1024] of String[32];
  StrEnc:array [1..1024] of String[32];
  StrDec:array [1..1024] of String[32];
end;
GCM = record
  {128Bit Key}

  {Now we are engag}
  StrKeyAsc:array [1..32] of String[16];
  {01234567890123456789012345678901
   4e6f772077652061726520656e676167}
  StrKeyHex:array [1..32] of String[32];
  Strplaintext:array [1..32] of String[16];
  StrData:array [1..32] of String[32];
  StrEnc:array [1..32] of String[32];
  StrDec:array [1..32] of String[32];
  StrIV:String[32];

  StrAAD: String[32];
  Actual:array [1..32] of String[80];
  Expected:array [1..32] of String[80];
  ActualTag:array [1..32] of String[80];
  ExpectedTag:array [1..32] of String[80];
end;

var img : TFPMemoryImage;
reader : TFPCustomImageReader;
Writer : TFPCustomimageWriter;
ReadFile, WriteFile, WriteOptions : string;
   INPUTFILETYPE,OUTPUTFILETYPE: String;
   INPUTFILE,OUTPUTFILE: String;
    WindowHandle:TWindowHandle;
    MyPLoggingDevice : ^TLoggingDevice;
     HTTPListener:THTTPListener;
    { needed to use ultibo-tftp  }
    TCP : TWinsock2TCPClient;
    IPAddress : string;

    h, w: Integer;
    clr: TFPColor;
    i, j: Integer;
    Red, Blue, Green : word;

 MyKey: AnsiString = '1234567890123456'; {Must be 16, 24 or 32 bytes}
  MyIV: AnsiString = 'My Secret IV';
  MyAAD: AnsiString = 'My Extra Secret AAD';
  MyData: AnsiString = 'The quick brown fox jumps over the lazy dog.The quick brown fox jumps over the lazy dog.';
  MyResult: AnsiString;
   Key: PByte;
  IV: PByte;
  AAD: PByte;
  Plain: PByte;
  Crypt: PByte;
  Tag: PByte;

  PCBC:^CBC;
  PGCM:^GCM;
  CBC1:CBC;
  GCM1:GCM;

  AESECBKey:PByte;

 AESECBData:PByte;
 AESECBAESKey:TAESKey;

 AESCBCKey:PByte;
 AESCBCData:PByte;
 AESCBCVector:PByte;

 AESGCMKey:PByte;
 AESGCMIV:PByte;
 AESGCMAAD:PByte;
 AESGCMData:PByte;
 AESGCMTag:PByte;

 Cipher:PCipherContext;


 Actual:String;

 PStrIV64:^Char;
 InKey:LongWord;
 InKeyStr:String;
 InDataStr:String;
 InIVStr:String;
 EncryptDecrypt:LongWord;

 Filename:String;
 StringList:TStringList;
 FileStream:TFileStream;
 S1,S2:String;
 xx : LongWord;




    function WaitForIPComplete : string;

   var

     TCP : TWinsock2TCPClient;

   begin

     TCP := TWinsock2TCPClient.Create;

     Result := TCP.LocalAddress;

     if (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') then

       begin

         while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do

           begin

             sleep (1500);

             Result := TCP.LocalAddress;

           end;

       end;

     TCP.Free;

   end;

procedure Msg (Sender : TObject; s : string);

begin

  ConsoleWindowWriteLn (WindowHandle, s);

end;



procedure WaitForSDDrive;

begin

  while not DirectoryExists ('C:\') do sleep (500);

end;


procedure Init;
var t : char;
begin
  if paramcount = 4 then
    begin
    T := upcase (paramstr(1)[1]);
    if T = 'X' then
      Reader := TFPReaderXPM.Create
    else if T = 'B' then
      Reader := TFPReaderBMP.Create
    else if T = 'J' then
      Reader := TFPReaderJPEG.Create
    else if T = 'P' then
      Reader := TFPReaderPNG.Create
    else if T = 'T' then
      Reader := TFPReaderTarga.Create
    else if T = 'N' then
      Reader := TFPReaderPNM.Create
    else
      begin
      Writeln('Unknown file format : ',T);
      //Halt(1);
      end;
    ReadFile := paramstr(2);
    WriteOptions := paramstr(3);
    WriteFile := paramstr(4);
    end
  else
    begin
    Reader := nil;
    ReadFile := paramstr(1);
    WriteOptions := paramstr(2);
    WriteFile := paramstr(3);
    end;
  WriteOptions := uppercase (writeoptions);
  T := WriteOptions[1];
  if T = 'X' then
    Writer := TFPWriterXPM.Create
  else if T = 'B' then
    begin
    Writer := TFPWriterBMP.Create;
    TFPWriterBMP(Writer).BitsPerPixel:=32;
    end
  else if T = 'J' then
    Writer := TFPWriterJPEG.Create
  else if T = 'P' then
    Writer := TFPWriterPNG.Create
  else if T = 'T' then
    Writer := TFPWriterTARGA.Create
  else if T = 'N' then
    Writer := TFPWriterPNM.Create
  else
    begin
    Writeln('Unknown file format : ',T);
    //Halt(1);
    end;
  img := TFPMemoryImage.Create(0,0);
  img.UsePalette:=false;
end;

procedure ReadImage;
{$ifndef UseFile}var str : TStream;{$endif}
begin
  if assigned (reader) then
    img.LoadFromFile (ReadFile, Reader)
  else
    {$ifdef UseFile}
    img.LoadFromFile (ReadFile);
    {$else}
    if fileexists (ReadFile) then
      begin
      str := TFileStream.create (ReadFile,fmOpenRead);
      try
        img.loadFromStream (str);
      finally
        str.Free;
      end;
      end
    else
      writeln ('File ',readfile,' doesn''t exists!');
    {$endif}
end;

procedure WriteImage;
var t : string;
begin
  t := WriteOptions;
  writeln (' WriteImage, options=',t);
  if (t[1] = 'P') then
    with (Writer as TFPWriterPNG) do
      begin
      Grayscale := pos ('G', t) > 0;
      Indexed := pos ('I', t) > 0;
      WordSized := pos('W', t) > 0;
      UseAlpha := pos ('A', t) > 0;
      writeln ('Grayscale ',Grayscale, ' - Indexed ',Indexed,
               ' - WordSized ',WordSized,' - UseAlpha ',UseAlpha);
      end
  else if (t[1] = 'X') then
    begin
    if length(t) > 1 then
    with (Writer as TFPWriterXPM) do
      begin
      ColorCharSize := ord(t[2]) - ord('0');
      end;
    end;
  writeln ('Options checked, now writing...');
  img.SaveToFile (WriteFile, Writer);
end;

procedure Clean;
begin
  Reader.Free;
  Writer.Free;
  Img.Free;
end;

begin
  { Add your program code here }

  {Create a console window as usual}
  WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

  ConsoleWindowWriteLn(WindowHandle,'Starting FPImage Imgconv');
    // Prompt for file type
 {ConsoleWindowWrite(WindowHandle,'Enter Input file type (X for XPM, P for PNG, B for BMP, J for JPEG, T for TGA): ');
 ConsoleWindowReadLn(WindowHandle,INPUTFILEType);
 ConsoleWindowWrite(WindowHandle,'Enter Output file type (X for XPM, P for PNG, B for BMP, J for JPEG, T for TGA): ');
 ConsoleWindowReadLn(WindowHandle,OUTPUTFILEType);
 ConsoleWindowWrite(WindowHandle,'Enter Input file ');
 ConsoleWindowReadLn(WindowHandle,INPUTFILE);
 ConsoleWindowWrite(WindowHandle,'Enter Output file ');
 ConsoleWindowReadLn(WindowHandle,OUTPUTFILE);
 ConsoleWindowWriteln(WindowHandle,  INPUTFILEType+' '+INPUTFILE);
 ConsoleWindowWriteln(WindowHandle,  OUTPUTFILEType+' '+OUTPUTFILE);}
  {Wait a couple of seconds for C:\ drive to be ready}
  ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
  while not DirectoryExists('C:\') do
   begin
    {Sleep for a second}
    Sleep(1000);
   end;
  // wait for IP address and SD Card to be initialised.
  WaitForSDDrive;
  IPAddress := WaitForIPComplete;
  {Wait a few seconds for all initialization (like filesystem and network) to be done}
  Sleep(5000);
  ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
  ConsoleWindowWriteLn(WindowHandle,'');


  ConsoleWindowWriteLn (WindowHandle, 'Local Address ' + IPAddress);
  SetOnMsg (@Msg);
  {Create and start the HTTP Listener for our web status page}
  {HTTPListener:=THTTPListener.Create;
  HTTPListener.Active:=True;
  {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
  WebStatusRegister(HTTPListener,'','',True);}
  ConsoleWindowWriteLn(WindowHandle,'Completed setting up WebStatus & IP');
  HTTPListener:=THTTPListener.Create;
  HTTPListener.Active:=True;
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);

 ConsoleWindowWriteLn(WindowHandle,'Initing');
 Reader := TFPReaderPNG.create;
 ConsoleWindowWriteLn(WindowHandle,'Reader bmp');
 Writer := TFPWriterBMP.Create;
 ConsoleWindowWriteLn(WindowHandle,'Writer png');

 ReadFile := 'lena_rgb_256.png';
 WriteFile := 'lena_rgb_256_fpng.bmp';
 WriteOptions := 'B';

 img := TFPMemoryImage.Create(0,0);



 img.UsePalette:=false;
 //img.UsePalette:=true; hangs at reader is assigned.


 ConsoleWindowWriteLn(WindowHandle,'  img create & UsePalette false');
 ConsoleWindowWriteLn(WindowHandle,'Calling ReadImage ReadFile '+ReadFile);
 if assigned (reader) then
    ConsoleWindowWriteLn(WindowHandle,'reader is assigned')
 else
    ConsoleWindowWriteLn(WindowHandle,'reader is not assigned');
 ReadImage;
   h:=img.Height;
 w:=img.Width;
 ConsoleWindowWriteLn(WindowHandle,'Height ' + intToStr(h)+' Width '+intToStr(w));
 for j := 0 to img.Height - 1 do
      for i := 0 to img.Width - 1 do
      begin
        clr := img.Colors[i, j];
        //R*0.29900 + Line[x].G*0.58700 + Line[x].B*0.11400

        clr.red:=round(clr.red*0.29900);
        clr.blue:=round(clr.blue*0.11400);
        clr.green:=round(clr.green*0.58700);
        clr.green:=clr.red+clr.blue+clr.green;
        clr.red:=0;
        clr.blue:=0;
        //ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+intToStr(j)+' '+intToStr(clr.red)+' '+intToStr(clr.blue) + ' ' +intToStr(clr.green) );
        //ConsoleWindowWriteLn(WindowHandle,intToStr(RED));
        //clr.Red=1;
        //((clr.Red / 65535)**AGamma)*65535);
        //clr.Green := round(((clr.Green / 65535)**AGamma)*65535);
        //clr.Blue := round(((clr.Blue / 65535)**AGamma)*65535);
        img.Colors[i, j] := clr;
      end;


 ConsoleWindowWriteLn(WindowHandle,'Calling WriteImage WriteFile '+WriteFile +' ' + WriteOptions);


 WriteImage;
 Clean;

 {Halt the main thread here}
 CBC1.StrKeyAsc:='Now we are engaged in a great ci';
  S1:=CBC1.StrKeyAsc;
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (WindowHandle, 'CBC1.StrKeyAsc ' + CBC1.StrKeyAsc);
  ConsoleWindowWriteLn (WindowHandle, 'CBC1.StrKeyHex ' + CBC1.StrKeyHex);
  ConsoleWindowWriteLn (WindowHandle,S2);

  xx := 52;

 ConsoleWindowWriteLn (WindowHandle,'first digit is ascii 4 52 dec is 00110100 ' + intToStr(xx));

 xx:= asciiValueToBinary(xx);
 ConsoleWindowWriteLn (WindowHandle,intToStr(xx));
 ThreadHalt(0);

end.


