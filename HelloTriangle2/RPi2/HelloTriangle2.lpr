program HelloTriangle2;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Triangle2                                       }
{                                                                              }
{ Using an OpenGL ES 2 shader to draw a mandelbrot set.                        }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ To see the full effect of this example you should connect a USB mouse to     }
{ your Raspberry Pi, move the mouse to redraw the Mandelbrot.                  }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B/3B+/3A+.      }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  Mouse,
  SysUtils,

  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  Classes,



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
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 X:Integer = 800; {A couple of variables for mouse position tracking}
 Y:Integer = 400;
 First:Boolean = True;
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

{Link our C library to include the original example} 
{$linklib hello_triangle2}
 
{Import the main function of the example so we can call it from Ultibo}
procedure hello_triangle2; cdecl; external 'hello_triangle2' name 'hello_triangle2';
 
{Export a function to allow the example access to the Ultibo mouse} 
function ultibo_get_mouse(Width,Height:Integer;MouseX,MouseY:PInteger): Integer; cdecl; public name 'ultibo_get_mouse'; 
var
 Count:LongWord;
 Buffer:TMouseData;
begin
 {}
 {In the Linux version of this example moving the mouse redraws the mandelbrot with new parameters
  based on the movement of the mouse. This function simply replaces the calls to /dev/input/mouse 
  with calls to Ultibo mouse functions}
  
 {Read a packet from the mouse}
 Count:=0;
 if not(First) and (MouseRead(@Buffer,SizeOf(TMouseData),Count) = ERROR_SUCCESS) then
  begin
   {Check for both left and right buttons}
   {The original example exits if either the left or right mouse button is pressed however
    the examples doesn't cleanup the OpenGL, EGL or DispmanX configuration so it simply stops
    with the screen as it was at that point. If you want to experiment uncomment these lines
    and try it out for yourself}
   (*if (Buffer.Buttons and (MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON)) <> 0 then
    begin
     Result:=1;
     Exit;
    end;*)
    
   X:=X + Buffer.OffsetX;
   Y:=Y + Buffer.OffsetY;
   
   if X < 0 then X:=0;
   if Y < 0 then Y:=0;
   
   if X > Width then X:=Width;
   if Y > Height then Y:=Height;
  end;
 
 First:=False;
 
 {Return results}
 MouseX^:=X;
 MouseY^:=Y;
 
 Result:=0;
end;

begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Triangle2');
 {Wait a couple of seconds for C:\ drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 {while not DirectoryExists('C:\') do
  begin
   {Sleep for a second}
   Sleep(1000);
  end;}

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
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_triangle2;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Triangle2');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

