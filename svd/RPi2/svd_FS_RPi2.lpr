program svd_FS_RPi2;

{$mode objfpc}{$H+}

uses
 RaspberryPi2, {<-- Change this to suit which model you have!!}
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 Console,
 SysUtils,  { TimeToStr & Time }

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

 Logging,
 Syscalls;

{$linklib svd}
{$linklib libm}

procedure test_svd; cdecl; external 'libsvd' name 'test_svd';

var
 MyPLoggingDevice : ^TLoggingDevice;
 Handle:THandle;
 Handle1:THandle;
 Handle3:THandle;
 {Handle2:THandle;}
 Window:TWindowHandle;


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

  ConsoleWindowWriteLn (Handle, s);

end;



procedure WaitForSDDrive;

begin

  while not DirectoryExists ('C:\') do sleep (500);

end;



begin


 // wait for IP address and SD Card to be initialised.
 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Wait a few seconds for all initialization (like filesystem and network) to be done}
 Sleep(5000);

{
 The following 3 lines are logging to the console
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
 }




 {Create a graphics window to display our bitmap, let's use the new CONSOLE_POSITION_FULLSCREEN option}
 {Window:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOM);}


 //test_svd();
 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
 ConsoleWindowWriteLn(Handle, TimeToStr(Time));
 //Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);
 ConsoleWindowWriteLn (Handle, 'TFTP Demo.');
 {Handle2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);}
 //Handle3:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMRIGHT,True);
//ConsoleWindowWriteLn(Handle1, 'writing top right handle1');
 {ConsoleWindowWriteLn(Handle2, 'writing bottom left handle2');}
 //ConsoleWindowWriteLn(Handle3, 'writing bottom right handle3');

 test_svd();
 ConsoleWindowWriteLn(Handle, TimeToStr(Time));




  ConsoleWindowWriteLn (Handle, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 ConsoleWindowWriteLn(Handle, TimeToStr(Time));



 ThreadHalt(0);
end.



