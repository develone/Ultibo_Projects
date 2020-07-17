program HelloJPEG;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello JPEG                                            }
{                                                                              }
{ Decoding a JPEG using the hardware JPEG decoder (note that this example is a }
{ decoder only and does not display the picture).                              }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ Make sure you also copy the Beetroot.jpg file from the Media folder.         }
{                                                                              }
{ Remember that this example only demonstrates decoding a JPEG in hardware and }
{ will not display the picture on screen.                                      }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B/3B+/3A+.      }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  SysUtils,
  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
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
 argc:int;      {Some command line arguments to pass to the C code}
 argv:PPChar;
 
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
{$linklib hello_jpeg}
 
{Import the main function of the example so we can call it from Ultibo}
function hello_jpeg(argc: int; argv: PPChar): int; cdecl; external 'hello_jpeg' name 'hello_jpeg';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello JPEG');

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
 {Allocate a command line for the C code, this function just takes a string and creates a
  properly formatted argv and argc which can be used to pass parameters to the example}
 argv:=AllocateCommandLine('C:\Beetroot.jpg',argc);
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_jpeg(argc, argv);

 {Release the C command line} 
 ReleaseCommandLine(argv);
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello JPEG');
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);
 {Halt the main thread here}
 ThreadHalt(0);
end.

