program DWT_LIFT_RPi2;

{$mode objfpc}{$H+}

uses
 RaspberryPi2, {<-- Change this to suit which model you have!!}
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 Console,
 HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
 WebStatus,
 SysUtils,  { TimeToStr & Time }
 { needed by bitmap }
 GraphicsConsole, {Include the GraphicsConsole unit so we can create a graphics window}
 BMPcomn,         {Include the BMPcomn unit from the fpc-image package to give the Bitmap headers}
 Classes,
 { needed by bitmap }
 { needed to use ultibo-tftp  }
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
 Syscalls;

{$linklib dwtlift}
{$linklib libm}

procedure decom_test(x0,y0,x1,y1:LongWord;fn:string); cdecl; external 'libdwtlift' name 'decom_test';
var

 Handle:THandle;
 Handle1:THandle;
 {Handle2:THandle;}
 Window:TWindowHandle;
 HTTPListener:THTTPListener;

 IPAddress : string;

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

 {The following 2 lines are logging to a file}
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));


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
 Window:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOM);

 {Call our bitmap drawing function and pass the name of our bitmap file on the SD card,
  we also pass the handle for our graphics console window and the X and Y locations to
  draw the bitmap.

  What happens if the bitmap is bigger than the window? It will be trimmed to fit, try it
  yourself and see}

 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);
 Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);
 ConsoleWindowWriteLn (Handle1, 'TFTP Demo.');
 {Handle2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);}
 //Handle3:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMRIGHT,True);
 ConsoleWindowWriteLn(Handle1, 'writing top right handle1');
 {ConsoleWindowWriteLn(Handle2, 'writing bottom left handle2');}
 //ConsoleWindowWriteLn(Handle3, 'writing bottom right handle3');
 ConsoleWindowWriteLn(Handle, TimeToStr(Time));

 DECOMP:=6;
 ENCODE:=1;
 //should not be set lower than  30 which is compressiong over 1500
 //
 //		38	189.4093899116
 //		44	44.058396563
 //		50	15.9377967826
 //		54	8.6079098426
 //		58	6.0368784486
 //		60	5.5454244973

 TCP_DISTORATIO:=60;
 //FILTER 0 5/3 DWT
 //FILTER 1 9/7 DWT
 FILTER:= 0;
 COMPRESSION_RATIO := 125;
 //DIS_CR_FLG 0 COMPRESSION_RATIO
 //DIS_CR_FLG 1 TCP_DISTORATIO
 DIS_CR_FLG := 0;
 if (ENCODE = 1) then

 DrawBitmap(Window,'C:\MyBitmap.bmp',0,0,DECOMP,ENCODE,TCP_DISTORATIO,FILTER, COMPRESSION_RATIO,DIS_CR_FLG);

 if(ENCODE = 0) then
 begin
 da_x0:=0;
 da_y0:=0;
 da_x1:=1024;
 da_y1:=1024;
 ff:='test.j2k';
 decom_test(da_x0,da_y0,da_x1,da_y1,ff);
 DrawBitmap(Window,'C:\test_wr.bmp',0,0,DECOMP,ENCODE,TCP_DISTORATIO,FILTER, COMPRESSION_RATIO,DIS_CR_FLG);
 end;
 ConsoleWindowWriteLn (Handle1, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 ConsoleWindowWriteLn(Handle, TimeToStr(Time));
 {-----------------------------------
 X:= 0;
 y:= 0;
 Width:= 1024;
 Height:= 1024;
  if SaveBitmap(Window,'C:\MySavedBitmap.bmp',X,Y,Width,Height,24) then
  begin
   {Output a message when the file is saved}
   GraphicsWindowDrawTextEx(Window,GraphicsWindowGetFont(Window),'Bitmap file saved successfully',260,100,COLOR_BLACK,COLOR_WHITE);

  end;
 -------------------------------------------}


 ThreadHalt(0);
end.


