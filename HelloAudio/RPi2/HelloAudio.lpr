program HelloAudio;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Audio                                           }
{                                                                              }
{ A very simple example showing audio output over HDMI                         }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B.              }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  SysUtils,  { TimeToStr & Time }
  Logging,
  uTFTP,
  Winsock2,
  { needed to use ultibo-tftp  }
  { needed for telnet }
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell,
  { needed for telnet }

  UltiboUtils,  {Include Ultibo utils for some command line manipulation}
  Syscalls,     {Include the Syscalls unit to provide C library support}
  VC4;          {Include the VC4 unit to enable access to the GPU}

var
 argc:int;      {Some command line arguments to pass to the C code}
 argv:PPChar;
 
 WindowHandle:TWindowHandle;
 HTTPListener:THTTPListener;
 { needed to use ultibo-tftp  }
 TCP : TWinsock2TCPClient;
 IPAddress : string;


{Link our C library to include the original example} 
{$linklib hello_audio}
 
{Import the main function of the example so we can call it from Ultibo}
function hello_audio(argc: int; argv: PPChar): int; cdecl; external 'hello_audio' name 'hello_audio';

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
 {The following 3 lines are logging to the console
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
 }

 {The following 2 lines are logging to a file
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE)); }

 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Audio');

  // wait for IP address and SD Card to be initialised.
 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 ConsoleWindowWriteLn (WindowHandle, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);

 {Allocate a command line for the C code, this function just takes a string and creates a
  properly formatted argv and argc which can be used to pass parameters to the example}
 argv:=AllocateCommandLine('1',argc);
 
 {Call the main function of the example, it will return here when completed (if ever)}
 hello_audio(argc, argv);

 {Release the C command line} 
 ReleaseCommandLine(argv);
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Audio');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

