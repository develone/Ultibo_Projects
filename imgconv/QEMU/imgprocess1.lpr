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
  APICrypto,
  uFromC


  { Add additional units here };

function asciiValueToBinary(x0:LongWord):LongWord; cdecl; external 'libcvtutils' name 'asciiValueToBinary';
procedure processstr(s:String); cdecl; external 'libcvtutils' name 'processstr';
//procedure ReturnFromProcessStr(Value: PChar); cdecl; public name 'returnfromprocessstr';

type
  MODR = array[0..255,0..255] of word;
  MODRPtr = ^MODR;
  XORR = array[0..255,0..255] of word;
  XORRPtr = ^XORR;
  TLSB = array[0..255,0..255] of word;
  TLSBPtr = ^TLSB;
  Lsb = array[0..31] of byte;
  lsbPtr = ^Lsb;
  Buffer = String[255];
  BufPtr = ^Buffer;

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

var img: TFPMemoryImage;
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
 databuffer :PChar;
 B  : Buffer;
 BP : BufPtr;
 PP : Pointer;
 bb : Lsb;
 bbp : LsbPtr;
 modbuf : MODR;
 xorbuf : XORR;
 tlsbbuf : TLSB;

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
 ConsoleWindowWriteLn(WindowHandle,'Reader png');
 Writer := TFPWriterPNG.Create;
 ConsoleWindowWriteLn(WindowHandle,'Writer png');

 ReadFile := 'input.png';
 WriteFile := 'GrayScale.png';
 WriteOptions := 'P';


 img := TFPMemoryImage.Create(0,0);



 img.UsePalette:=false;
 //img.UsePalette:=true; hangs at reader is assigned.


 ConsoleWindowWriteLn(WindowHandle,'  img create & UsePalette false');
 ConsoleWindowWriteLn(WindowHandle,'Calling ReadImage ReadFile '+ReadFile);
 if assigned (reader) then
    ConsoleWindowWriteLn(WindowHandle,'img reader is assigned')
 else
    ConsoleWindowWriteLn(WindowHandle,'img reader is not assigned');
 ReadImage;


 CBC1.StrKeyAsc:='Now we are engaged in a great ci';
 //CBC1.StrKeyAsc:='23AE14F4A7B2DC7F1DD89CF6F07E4048';
  S1:=CBC1.StrKeyAsc;
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (WindowHandle, 'CBC1.StrKeyAsc ' + CBC1.StrKeyAsc);
  ConsoleWindowWriteLn (WindowHandle, 'CBC1.StrKeyHex ' + CBC1.StrKeyHex);
  ConsoleWindowWriteLn (WindowHandle,S2);



  B:=S1;
  BP:=@B;
  //ConsoleWindowWriteLn (WindowHandle,'checking that BP points to string '+BP^[1]+BP^[2]+BP^[3]);
  ConsoleWindowWriteLn (WindowHandle,'This is the data in the buffer B '+B);
  ConsoleWindowWriteLn (WindowHandle,'Setting PP to the value of BP the BufPtr ');
  PP:=BP;
  ConsoleWindowWriteLn (WindowHandle,'PP is the pointer passed to returnfromprocessstr ');

 processstr('Now we are engaged in a great ci');
 //ConsoleWindowWriteLn(WindowHandle,'ProcessStrResult = ' + ProcessStrResult);
 i:=length(ProcessStrResult);
 B:=ProcessStrResult;
 BP:=@B;
 bbp:=@bb;
 i:=8;
 while(i<256) do
 begin
      S1:=BP^[i];
      bbp^[i-8]:=strToInt(S1);
      ConsoleWindowWrite(WindowHandle,intToStr(i)+' '+S1+' '+intToStr(bbp^[i-8]) +' ');
      i:=i+8;

 end;

 ConsoleWindowWriteLn(WindowHandle,' ');

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
        clr.red:=clr.green;
        clr.blue:=clr.green;
        modbuf[i,j] := clr.red mod 2;
        xorbuf[i,j] := modbuf[i,j] xor 0;
        {x mod y 42668 (0) even 0 odd 1 40859 (1)
        ModRed 0 XORRed 1 temp 1 ModRed 1 XORRed 1 temp 0
        ModRed 0 XORRed 0 temp 0 ModRed 1 XORRed 0 temp 1}
        ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+intToStr(j)+' '+intToStr(modbuf[i,j])+' '+intToStr(xorbuf[i,j])+' '+intToStr(clr.red));

        img.Colors[i, j] := clr;
      end;


  ConsoleWindowWriteLn(WindowHandle,'Calling WriteImage WriteFile '+WriteFile +' ' + WriteOptions);

 WriteImage;

 Clean;

 ThreadHalt(0);

end.


