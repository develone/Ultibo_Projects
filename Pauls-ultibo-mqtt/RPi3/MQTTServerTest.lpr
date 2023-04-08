program MQTTServerTest;

{$mode objfpc}{$H+}
{$define use_tftp}

uses

  RaspberryPi3,
  BCM2837,
  BCM2710,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Syscalls,
  DateUtils,

 FileSystem,  {Include the file system core and interfaces}
 FATFS,       {Include the FAT file system driver}
 MMC,         {Include the MMC/SD core to access our SD card}
  Threads,

  SysUtils,
  Classes, Console,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
   { needed for telnet }
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell,
  { needed for telnet }
  Ultibo, uMQTTServer, winsock2, uLog, uTFTP, uMQTT
  { Add additional units here };

type

  { THelper }

  THelper = class
    i : integer;
    procedure MQCheckUser (Sender : TObject; aUser, aPass : UTF8String; var Allowed : Boolean);
    procedure MQObituary (Sender : TObject; var aTopic, aMessage : UTF8String; var aQos : TMQTTQOSType);
    procedure MQSubscription (Sender : TObject; aTopic : UTF8String; var RequestedQos : TMQTTQOSType);
    procedure MQCMsg (Sender : TObject; aTopic : UTF8String; aMessage : AnsiString; aQos : TMQTTQOSType; aRetained : boolean);

  end;


var
  Console1, Console2, Console3 : TWindowHandle;
  Counter:LongWord;
{$ifdef use_tftp}
  IPAddress : string;
  pubtime : string;
{$endif}




  ch : char;
  MQ : TMQTTServer;
  MQC : TMQTTClient;
  Helper : THelper;
  MQT : TMQTTThread;

procedure Log1 (s : string);
begin
  ConsoleWindowWriteLn (Console1, s);
end;

procedure Log2 (s : string);
begin
  ConsoleWindowWriteLn (Console2, s);
end;

procedure Log3 (s : string);
begin
  ConsoleWindowWriteLn (Console3, s);
end;

procedure Msg2 (Sender : TObject; s : string);
begin
  Log2 ('TFTP : ' + s);
end;

{$ifdef use_tftp}
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
{$endif}

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

{ THelper }

procedure THelper.MQCheckUser (Sender: TObject; aUser, aPass: UTF8String;
  var Allowed: Boolean);
begin
   Log ('  Check User ' + aUser + ' pass ' + aPass);
   Allowed := true;
end;

procedure THelper.MQObituary (Sender : TObject; var aTopic, aMessage : UTF8String; var aQos : TMQTTQOSType);
begin
  Log ('  Obituary ' + aTopic + '  ' + aMessage + ' @ ' + QOSNames[aQos]);
end;

procedure THelper.MQSubscription (Sender : TObject; aTopic : UTF8String; var RequestedQos : TMQTTQOSType);
begin
  Log ('  Subscription "' + aTopic + '" @ ' + QOSNames[RequestedQos]);
end;

procedure THelper.MQCMsg (Sender : TObject; aTopic : UTF8String; aMessage : AnsiString; aQos : TMQTTQOSType; aRetained : boolean);
var
  x : integer;
  l : integer;
begin
  Log ('  Topic "' + aTopic + '" @ ' + QOSNames[aQos] + ' Retained ' + ny[aRetained]);
  if aTopic = 'update/memo' then
    begin
      x := 1;
      while x + 1 <= length (aMessage) do
        begin
          l := ord (aMessage[x]) * $100 + ord (aMessage[x + 1]);
          if x + l + 1 <= length (aMessage) then
            Log3 (Copy (aMessage, x + 2, l));
          x := x + l + 2;
        end;
    end;
end;

begin
  Console1 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_LEFT, true);
  Console2 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_TOPRIGHT, false);
  Console3 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOMRIGHT, false);
  SetLogProc (@Log1);
   {Initialize a variable so we can count how long we've been waiting}
 Counter:=0;

 {Let's wait for a while for the time to be updated}
 while YearOf(Now) < 2000 do
  begin
   {Sleep for a second}
   Sleep(1000);

   {Update our counter}
   Inc(Counter);

   {Check how long we have waited}
   if Counter > 90 then
    begin
     {Print a failure message on the console}
     ConsoleWindowWriteLn(Console3,'Sorry, failed to get the time after 90 seconds. Is the network connected?');

     {Break out of the loop and continue}
     Break;
    end;
  end;
  ConsoleWindowWriteLn(Console3,'The date and time is ' + FormatDateTime('yyyy-mm-dd-hh-mm-ss',Now));
  pubtime:=   FormatDateTime('yyyy-mm-dd-hh-mm-ss',Now);
  Log3 ('MQTT Client & Server Tester.');
  Log3 ('');
  WaitForSDDrive;
{$ifdef use_tftp}
  IPAddress := WaitForIPComplete;
  Log2 ('TFTP : Usage tftp -i ' + IPAddress + ' put kernel7.img');
  SetOnMsg (@Msg2);
  Log2 ('');
{$endif}
  MQ := TMQTTServer.Create;
  Helper := THelper.Create;
  Helper.i := 0; // suppress note
  MQ.OnCheckUser := @Helper.MQCheckUser;
  MQ.OnObituary := @Helper.MQObituary;
  MQ.OnSubscription := @Helper.MQSubscription;
  MQC := TMQTTClient.Create;
  MQC.OnMsg := @Helper.MQCMsg;
  ch := #0;
  while true do
    begin
      if ConsoleGetKey (ch, nil) then
        case (UpperCase (ch)) of
          '1' : MQ.SetTimer (1, 8000, true);
          '2' : MQ.SetTimer (2, 1000, false);
          '3' : MQ.KillTimer (1);
          '4' : MQ.KillTimer (2);
          '5' : MQ.Activate (true);
          '6' : MQ.Activate (false);
          '7' :
            begin
              MQC.Host := '192.168.1.231';
              MQC.Username := 'testuser';
              MQC.Password := 'password123';
              MQC.LocalBounce := false;
              MQC.Activate (true);
            end;
          '8' : MQC.Activate (false);
          '9' :
            begin
              MQC.Subscribe ('update/memo', qtEXACTLY_ONCE);
              MQC.Subscribe ('update/png/+', qtEXACTLY_ONCE);
              MQC.Subscribe ('will/#', qtEXACTLY_ONCE);
            end;
          '0' : MQC.Publish ('pub_time', pubtime, qtEXACTLY_ONCE, false);
          'Q' :
            begin
              MQT := TMQTTThread (MQ.Threads.First);
              while (MQT <> nil) do
                begin
                  MQT.Server.Disconnect;
                  MQT := TMQTTThread (MQT.Next);
                end;
            end;
        end;
    end;
  ThreadHalt (0);
end.





