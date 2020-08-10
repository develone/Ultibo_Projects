program ultibointerpol;
// Interpolation demo for fcl-image by Bernd Kreuss. Mantis #22245
// Loads original.png (not included) and scales it back to 64x64

{$mode objfpc}{$H+}

uses
  RaspberryPi3,
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
  Ultibo,

  Classes,

  FPimage,
  FPImgCanv,
  FPReadBMP,
  FPWriteBMP,
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
  Syscalls     {Include the Syscalls unit to provide C library support}
  { Add additional units here };
var
  ImOriginal: TFPMemoryImage;
  ImScaled: TFPMemoryImage;
  CanvScaled: TFPImageCanvas;
  Reader: TFPReaderBMP;
  Writer: TFPWriterBMP;
  WindowHandle:TWindowHandle;
  MyPLoggingDevice : ^TLoggingDevice;
   HTTPListener:THTTPListener;
  { needed to use ultibo-tftp  }
  TCP : TWinsock2TCPClient;
  IPAddress : string;

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

begin
  { Add your program code here }

    {Create a console window as usual}
    WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

    ConsoleWindowWriteLn(WindowHandle,'Starting FPImage Interpol');

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

  ImOriginal := TFPMemoryImage.Create(0, 0);
  ImScaled := TFPMemoryImage.Create(640, 480);
  Reader := TFPReaderBMP.create;
  Writer := TFPWriterBMP.create;
  //Writer.UseAlpha := True;
  ImOriginal.LoadFromFile('lena_rgb_1024.bmp', Reader);

  CanvScaled := TFPImageCanvas.create(ImScaled);
  CanvScaled.StretchDraw(0,0,639,479, ImOriginal);

  ImScaled.SaveToFile('lena640480.bmp', Writer);
  Reader.Free;
  Writer.Free;
  ImOriginal.Free;
  ImScaled.Free;
  ConsoleWindowWriteLn (WindowHandle, 'Interpol Complete');
  {Halt the main thread here}
  ThreadHalt(0);
end.

