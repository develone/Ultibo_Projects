program PicoTest1;

{$mode objfpc}
{$H+}
{$hints off}
{$notes off}

{ This demo uses a simple state machine to handle the serial cable }
{   being connected and disconnected from the computer.            }
{ }

uses
  RaspberryPi3,
  GlobalConfig,   //Add the global config unit
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  SysUtils,
  Logging,      //Add the logging unit
  Serial,   {Include the Serial unit so we can open, read and write to the device}
  uTFTP,
  Winsock2,
  USBCDCACM,
  Services;

const
  stUnknown = 0;
  stFind    = 1;
  stOpen    = 2;
  stOpened  = 3;
  stClose   = 4;
  stClosed  = 5;

var
  Count : LongWord;
  ch : char;
  Characters : string;
  s : string;
  Console1 : TWindowHandle;
  Pico : PSerialDevice;
  res : LongWord;
  state, oldState : integer;
  IPAddress : string;
  LogDevice : PLoggingDevice;

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

function Display (s : string) : string;
var
  i : integer;
begin
  Result := '';
  for i := 1 to length (s) do
    if s[i] in [' ' .. '~'] then
      Result := Result + s[i]
    else
      Result := Result + '<' + IntToStr (ord (s[i])) + '>';
end;

procedure Log (s : string);
begin
  ConsoleWindowWriteLn (Console1, s);
end;

procedure Msg (Sender : TObject; s : string);
begin
  Log (s);
end;

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

begin
  LogDevice := LoggingDeviceFindByType (LOGGING_TYPE_SYSLOG);
  SysLogLoggingSetTarget (LogDevice, '192.168.1.211');
  LoggingDeviceSetDefault (LogDevice);
  Console1 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, true);
  WaitForSDDrive;
  IPAddress := WaitForIPComplete;
  Log ('Welcome to PIco Test Program II.');
  oldState := stUnknown;
  state := stFind;
  Count := 0;
  Characters := '';
  while true do
    begin
      if state <> oldState then
        begin
          case state of
            stUnknown : s := 'Unknown';
            stFind    : s := 'Finding';
            stOpen    : s := 'Opening';
            stOpened  : s := 'Opened';
            stClose   : s := 'Closing';
            stClosed  : s := 'Closed';
            else        s := '????';
            end;
          Log ('Serial Port State ' + s);
          oldState := state;
        end;
      case state of
        stFind :
          begin
            Pico :=  SerialDeviceFindByDescription ('USB CDC ACM Serial');
            if Pico <> nil then state := stOpen;
          end;
        stOpen :
          begin
            sleep (1000);
            res := SerialDeviceOpen (Pico, 115200, SERIAL_DATA_8BIT, SERIAL_STOP_1BIT, SERIAL_PARITY_NONE, SERIAL_FLOW_DSR_DTR, 0, 0);
            if res = ERROR_SUCCESS then state := stOpened
            else if res = ERROR_INVALID_PARAMETER then state := stFind;
          end;
        stOpened :
          begin
            res := SerialDeviceRead (Pico, @ch, SizeOf (ch), SERIAL_READ_NON_BLOCK, Count);
            if (res = ERROR_SUCCESS) and (Count > 0) then  // non blocking so count may be 0
              begin
                if ch = #13 then
                  begin
                    Log ('Received a line: "' + Display (Characters) + '"');
                    Characters := '';
                  end
                else
                  Characters := Characters + ch;
              end
            else if res = ERROR_INVALID_PARAMETER then
              begin
                if Pico <> nil then SerialDeviceClose (Pico);
                Pico := nil;
                state := stFind;
              end;
          end;
        end; // case
      sleep (100);
    end; // true;
  { Halt the thread if we exit the loop }
  ThreadHalt (0);
end.


