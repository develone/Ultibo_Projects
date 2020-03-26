program SlipDriverDev;

{$mode delphi}{$H+}

uses
  RaspberryPi2,
  GlobalConst,
  Threads,
  Classes,
  SysUtils,
  Console,
  {Include HTTP and WebStatus so we can see from a web browser what is happening}
  HTTP,
  WebStatus,
  {Include HTTP and WebStatus so we can see from a web browser what is happening}
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

  uMySlip;
type
  Buffer = String[255];
  BufPtr = ^Buffer;
var
 Handle:THandle;
 Handle1:THandle;
 Handle2:THandle;

 OpenStatus:LongWord;
 CloseStatus:LongWord;

 Count:LongWord;
 B  : Buffer;
 BP : BufPtr;


 { var needed to support webstatus tftp & telnet}
 TCP : TWinsock2TCPClient;
 IPAddress : string;
 HTTPListener:THTTPListener;

 { var needed to support webstatus tftp & telnet}



{ functions & procedures needed to support tftp & telnet}



function WaitForIPComplete : string;
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
{ functions & procedures needed to webstatus support tftp & telnet}



begin
 { initialize to support webstatus tftp & telnet}

 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 Sleep(5000);
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);

   {Create our console window}
  Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

 Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);

 Handle2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);

 {Testing MySlip functions B:='ABC1234567890ABC1234567890';}

 B:='ABC1234567890ABC1234567890';
 OpenStatus:=0;
 OpenStatus := MySlipOpen(Handle2);
 ConsoleWindowWriteLn(Handle1,'MySlipOpen Status should be 1 if success');
 ConsoleWindowWriteLn(Handle1,'MySlipOpen Status should be 0 if failed to open');
 ConsoleWindowWriteLn(Handle1,'MySlipOpen Status ' + intToStr(OpenStatus));

 {Setting the Count to N writes N characters of string}
 if OpenStatus = 1 then
   B:='ABC12345';
   B:=B +  Chr(13) + Chr(10);
   begin
    Count:=10;
    BP := @B;
    ConsoleWindowWriteln(Handle2, B + ' '+ intToStr(SizeOf(BP)));
    Count:=MySlipTransmit(Handle2,BP,Count);
    ConsoleWindowWriteLn(Handle1,'num wrote ' + intToStr(Count));
   end;

 {Setting the Count to Length(B) writes the entire string}
 if OpenStatus = 1 then
   begin
     B:='ABC1234567890ABC1234567890';
   B:=B +  Chr(13) + Chr(10);
   Count:=Length(B);
   BP := @B;
   ConsoleWindowWriteln(Handle2, B + ' '+ intToStr(SizeOf(BP)));
   Count:=MySlipTransmit(Handle2,BP,Count);
   ConsoleWindowWriteLn(Handle1,'num wrote ' + intToStr(Count));
   end;

 sleep(2000);
 CloseStatus:=0;
 CloseStatus := MySlipClose(Handle2);
 ConsoleWindowWriteLn(Handle1,'MySlipClose Status should be 1 if success');
 ConsoleWindowWriteLn(Handle1,'MySlipClose Status should be 0 if failed to close');
 ConsoleWindowWriteLn(Handle1,'MySlip Status ' + intToStr(CloseStatus));
 OpenStatus:= CloseStatus;

 if OpenStatus = 0 then
    begin
      ConsoleWindowWriteLn(Handle1,'Serial Port Closed');
    end
    else
    begin

      Count:=10;
      BP := @B;
      ConsoleWindowWriteln(Handle2, B + ' '+ intToStr(SizeOf(BP)));
      Count:=MySlipTransmit(Handle2,BP,Count);
      ConsoleWindowWriteLn(Handle1,'num wrote ' + intToStr(Count));
    end;


 { initialize to support webstatus tftp & telnet}


  ConsoleWindowWriteLn(Handle,'Starting Development of SLIPDriver');
  ConsoleWindowWriteLn(Handle, 'With support for tftp & telnet: ' + TimeToStr(Time));
  {Wait a couple of seconds for C:\ drive to be ready}
  ConsoleWindowWriteLn(Handle,'Waiting for drive C:\');
  while not DirectoryExists('C:\') do
   begin
    {Sleep for a second}
    Sleep(1000);
   end;
  ConsoleWindowWriteLn(Handle,'C:\ drive is ready');
  ConsoleWindowWriteLn(Handle,'');

  ConsoleWindowWriteLn (Handle1, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 ConsoleWindowWriteLn(Handle1, TimeToStr(Time));
 ThreadHalt(0);
end.


