program fcltest;

{$mode objfpc}{$H+}

uses
  //RaspberryPi,
  QEMUVersatilePB,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
 HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
 WebStatus,
 BMPcomn,         {Include the BMPcomn unit from the fpc-image package to give the Bitmap headers}
  Classes,
  Console,
  GraphicsConsole,
 uTFTP,
 Winsock2,
 { needed to use ultibo-tftp  }
 { needed for telnet }
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell,
  { needed for telnet }
 uLiftBitmap,
 Logging,
 Syscalls,
 FileSystem,  {Include the file system core and interfaces}
 FATFS,       {Include the FAT file system driver}
 MMC,         {Include the MMC/SD core to access our SD card}

  FPimage,
  FPReadPNG,

  uCanvas,

  FrameBuffer,
  freetypeh,
  Ultibo;
  {$linklib dwtlift}
  {$linklib libm}
procedure xx(x0:Pointer); cdecl; external 'libdwtlift' name 'xx';
procedure decom_test(x0,y0,x1,y1:LongWord;fn:string;asize:LongWord;abuffer:Pointer); cdecl; external 'libdwtlift' name 'decom_test';
procedure decom_disp(x0,y0,x1,y1,asize:LongWord;abuffer:Pointer); cdecl; external 'libdwtlift' name 'decom_disp';
  // Add your unit created from Bin2Type data
  //MyData

  { Add additional units here }

{$INCLUDE '100-1com.inc'}
//{$INCLUDE '128.inc'}
//{$INCLUDE '256.inc'}

// Add the new class
type
  TBufferStream = class(TCustomMemoryStream)
  public
    constructor Create(ABuffer: Pointer; ASize: PtrInt);
  end;

constructor TBufferStream.Create(ABuffer: Pointer; ASize: PtrInt);
begin
  inherited Create;

  SetPointer(ABuffer, ASize);
end;

const
  BACK_COLOUR                    = $FF055A93;

var
 PTBinaryData:^TBinaryData;
 Count:Integer;
 Filename:String;
 SearchRec:TSearchRec;
 StringList:TStringList;
 FileStream:TFileStream;
 WindowHandle:TWindowHandle;
 CR, enc, xx0, yy0, xx1, yy1:LongWord;
 MyPLoggingDevice : ^TLoggingDevice;
  Handle:THandle;
 Handle1:THandle;
  GConsole : TWindowHandle;
 Window:TWindowHandle;
 HTTPListener:THTTPListener;
 BGnd, aCanvas : TCanvas;
 BufferStream: TBufferStream;
 IPAddress : string;
  anImage0 : TFPCustomImage;
  DefFrameBuff : PFrameBufferDevice;
  Properties : TWindowProperties;
  
 DECOMP: Integer;
 ENCODE: Integer;
 TCP_DISTORATIO: Integer;
 FILTER: Integer;
 COMPRESSION_RATIO : Integer;
 DIS_CR_FLG : Integer;
 X:LongWord;
 Y:LongWord;
 Width:LongWord;
 Height:LongWord;
 da_x0,da_y0,da_x1,da_y1:LongWord;
 ff:string;
//{$INCLUDE 'hexdump.inc'}

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

  ConsoleWindowWriteLn (Handle1, s);

end;

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

begin
{
 The following 3 lines are logging to the console
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
 }

 {The following 2 lines are logging to a file
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));
 MyPLoggingDevice:=LoggingDeviceGetDefault;
 LoggingDeviceRedirectOutput(MyPLoggingDevice);}


 // wait for IP address and SD Card to be initialised.
 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 {Wait a few seconds for all initialization (like filesystem and network) to be done}
 Sleep(5000);
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);
 {Create a graphics window to display our bitmap, let's use the new CONSOLE_POSITION_FULLSCREEN option}
 Window:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT);
  Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);
  Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);
  GConsole := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOMRIGHT, true);
  ConsoleWindowWriteLn(Handle, TimeToStr(Time));
  ConsoleWindowWriteLn (Handle1, 'TFTP Demo.');

  DefFrameBuff := FramebufferDeviceGetDefault;
  DECOMP:=6;
 ENCODE:=1;
  //FILTER 0 5/3 DWT
 //FILTER 1 9/7 DWT
 FILTER:= 0;
 COMPRESSION_RATIO := 125;

 ConsoleWindowWriteLn(Handle, 'ENCODE: ' + intToStr(ENCODE));
 ConsoleWindowWriteLn(Handle, 'sizeof ENCODE: ' + intToStr(sizeof(ENCODE)));
  da_x0:=0;
 da_y0:=0;
 da_x1:=2048;
 da_y1:=2048;
 ff:='test.j2k';
 //rd_inps();
 {starting the procedure to read the file testfile which contains a struct
 which has the varables for decompression.}

 try
  Filename:='C:\testfile';
  try
   FileStream:=TFileStream.Create(Filename,fmOpenRead);
   FileStream.Read(CR,sizeof(CR));
   COMPRESSION_RATIO:=CR;
   ConsoleWindowWriteLn(Handle1, 'CR ' + intToStr(CR));
   FileStream.Read(enc,sizeof(enc));
   ENCODE:=enc;
   ConsoleWindowWriteLn(Handle1, 'enc ' + intToStr(enc));
   FileStream.Read(xx0,sizeof(xx0));
   da_x0:=xx0;
   ConsoleWindowWriteLn(Handle1, 'xx0 ' + intToStr(xx0));
   FileStream.Read(yy0,sizeof(yy0));
   da_y0:=yy0;
   ConsoleWindowWriteLn(Handle1, 'yy0 ' + intToStr(yy0));
   FileStream.Read(xx1,sizeof(xx1));
   da_x1:=xx1;
   ConsoleWindowWriteLn(Handle1, 'xx1 ' + intToStr(xx1));
   FileStream.Read(yy1,sizeof(yy1));
   da_y1:=yy1;
   ConsoleWindowWriteLn(Handle1, 'yy1 ' + intToStr(yy1));
   FileStream.Free;

  finally
  end;
 except
   on E: Exception do
   begin
     ConsoleWindowWriteLn(Handle, 'Error: ' + E.Message);
    end;
 end;

 PTBinaryData:=@BinaryData;
 ConsoleWindowWriteLn(Handle1, 'ASize ' + intToStr(SizeOf(BinaryData)));
 //ConsoleWindowWriteLn(Handle, 'ABuffer ' + IntToHex(PtrUInt(@BinaryData),8));
 ConsoleWindowWriteLn(Handle1, 'ABuffer ' + PtrToHex(PTBinaryData));
 //ConsoleWindowWriteLn(Handle, 'ABuffer ' + PtrToHex(@BinaryData));
 //xx(PTBinaryData);
 //decom_disp(da_x0,da_y0,da_x1,da_y1,SizeOf(BinaryData),PTBinaryData);

 //should not be set lower than  30 which is compressiong over 1500
 //
 //		38	189.4093899116
 //		44	44.058396563
 //		50	15.9377967826
 //		54	8.6079098426
 //		58	6.0368784486
 //		60	5.5454244973

 TCP_DISTORATIO:=60;
 //DIS_CR_FLG 0 COMPRESSION_RATIO
 //DIS_CR_FLG 1 TCP_DISTORATIO
 DIS_CR_FLG := 0;

 if (ENCODE = 1) then

 DrawBitmap(Window,'C:\MyBitmap.bmp',0,0,DECOMP,ENCODE,TCP_DISTORATIO,FILTER, COMPRESSION_RATIO,DIS_CR_FLG);


 if(ENCODE = 0) then
 begin

 decom_test(da_x0,da_y0,da_x1,da_y1,ff,SizeOf(BinaryData),PTBinaryData);
 DrawBitmap(Window,'C:\test_wr.bmp',0,0,DECOMP,ENCODE,TCP_DISTORATIO,FILTER, COMPRESSION_RATIO,DIS_CR_FLG);
 end;
 ConsoleWindowWriteLn (Handle1, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 ConsoleWindowWriteLn(Handle, TimeToStr(Time));
  aCanvas := TCanvas.Create;
  if GraphicsWindowGetProperties (GConsole, @Properties) = ERROR_SUCCESS then
    begin
      aCanvas.Left := Properties.X1;
      aCanvas.Top := Properties.Y1;
      aCanvas.SetSize (Properties.X2 +1 - Properties.X1 , Properties.Y2 + 1  - Properties.Y1  , COLOR_FORMAT_ARGB32);
   end;
  aCanvas.Fill (BACK_COLOUR);
  aCanvas.Flush (DefFrameBuff);

  anImage0 := TFPMemoryImage.Create (0, 0);

  // Remove the LoadFromFile
  //anImage0.LoadFromFile ('C:\background.png');

  // Add a LoadFromStream
  BufferStream := TBufferStream.Create(@BinaryData, SizeOf(BinaryData));
  //anImage0.LoadFromStream(BufferStream);

  BGnd := TCanvas.Create;
  BGnd.SetSize (aCanvas.Width, aCanvas.Height, aCanvas.ColourFormat);

  //aCanvas.DrawImage (anImage0, 0, 0, BGnd.Width, BGnd.Height);

  //aCanvas.Flush (DefFrameBuff);


  ThreadHalt (0);


end.

