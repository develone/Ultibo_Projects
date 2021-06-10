program AsyncTest;

{$mode objfpc}{$H+}

{ Raspberry Pi 3 Application    }
{  Async TCP Socket test }

uses
  RaspberryPi4,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  SyncObjs,
  Console,
  Winsock2,
  Keyboard,
  DWCOTG,
  uAsync,
  uTFTP,
    { needed to use ultibo-tftp  }
  { needed for telnet }

      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell,
  { needed for telnet }
  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC,         {Include the MMC/SD core to access our SD card}
  BCM2711,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus
  { Add additional units here };

var
  aSocket : TAsyncSocket;
  WindowHandle : TWindowHandle;
  IPAddress : string;
  ch : char;

function display_string (s : string) : string;
  var
    i : integer;
  begin
    Result := '';
    for i := 1 to length (s) do
      if s[i] in [' '..'~'] then
        Result := Result + s[i]
      else
        Result := Result + '[' + IntToHex (ord (s[i]), 2) + ']';
  end;

procedure Connected (Sender : TObject);
begin
  ConsoleWindowWriteLn (WindowHandle, 'Connected.');
end;

procedure Closed (Sender : TObject);
begin
  ConsoleWindowWriteLn (WindowHandle, 'Closed.');
end;

procedure Read (Sender : TObject; Buff : pointer; BuffSize : integer);
var
  s : string;
begin
  SetLength (s, BuffSize);
  Move (Buff^, PChar (s)^, BuffSize);
  ConsoleWindowWriteLn (WindowHandle, 'Read "' + display_string (s) + '"  ' +  IntToStr (BuffSize) + ' bytes.');
end;

procedure Msgs (Sender : TObject; s : string);
begin
  ConsoleWindowWriteLn (WindowHandle, 'TCP -> ' + s);
end;

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
          sleep (1000);
          Result := TCP.LocalAddress;
        end;
    end;
  TCP.Free;
end;

begin
  // open console window
  WindowHandle := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, false);
  ConsoleWindowWriteLn (WindowHandle, 'Async Socket Demo.');

  ch := #0;

  // wait for IP address to be determined
  IPAddress := WaitForIPComplete;
  ConsoleWindowWriteLn (WindowHandle, 'Local Address ' + IPAddress);

  // create async socket
  aSocket := TAsyncSocket.Create (@Msgs);
  // connect events
  aSocket.OnConnect := @Connected;
  aSocket.OnClose := @Closed;
  aSocket.OnRead:= @Read;
  // set port and addr
  aSocket.Addr := '192.168.1.245';
  aSocket.Port := 5050;
  aSocket.Connect;

  while true do
    begin
      if ConsoleReadChar (ch, nil) then
        case (ch) of
          'C', 'c' : aSocket.Connect;
          'D', 'd' : aSocket.Disconnect;
          'S', 's' : aSocket.Send ('Hello There.' + #13);
        end;
    end;
  ThreadHalt (0);
end.
