program HelloTiger;

{$mode objfpc}{$H+}

{ VideoCore IV example - Hello Tiger                                           }
{                                                                              }
{ A basic OpenVG example showing a rotating picture of a tiger, this example   }
{ uses a picture embedded in the code.                                         }
{                                                                              }
{ To compile the example select Run, Compile (or Run, Build) from the menu.    }
{                                                                              }
{ Once compiled copy the kernel7.img file to an SD card along with the         }
{ firmware files and use it to boot your Raspberry Pi.                         }
{                                                                              }
{ This version is for Raspberry Pi 2B and will also work on a 3B/3B+/3A+.      }

uses
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
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
{$linklib hello_tiger}
 
{Import the main function of the example so we can call it from Ultibo}
procedure hello_tiger; cdecl; external 'hello_tiger' name 'hello_tiger';
 
begin
 {Create a console window as usual}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 ConsoleWindowWriteLn(WindowHandle,'Starting Hello Tiger');

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
 hello_tiger;
 
 ConsoleWindowWriteLn(WindowHandle,'Completed Hello Tiger');
 
 {Halt the main thread here}
 ThreadHalt(0);
end.

