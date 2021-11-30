program picohelloworld ;

{$mode objfpc}
{$H+}

{Serial Connection USB FTDI                                        }
{   based on Ultibo demo                                           }
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
  Framebuffer,
  BCM2837,
  BCM2710,
  SysUtils,
  Logging,      //Add the logging unit
  FTDISerial,
  Serial;   {Include the Serial unit so we can open, read and write to the device}

const
  stUnknown = 0;
  stFind    = 1;
  stOpen    = 2;
  stOpened  = 3;
  stClose   = 4;
  stClosed  = 5;

var
  Count : LongWord;
  Character : char;
  Characters : string;
  WindowHandle : TWindowHandle;
  SerialDevice : PSerialDevice;
  res : LongWord;

  state, oldState : integer;
  s : string;

begin
  WindowHandle := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, true);
  ConsoleWindowWriteLn (WindowHandle, 'Welcome to PIco Test Program.');
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
          ConsoleWindowWriteLn (WindowHandle, 'Serial Port State ' + s);
          oldState := state;
        end;
      case state of
        stFind :
          begin
            SerialDevice :=  SerialDeviceFindByDescription('USB CDC ACM Serial');


            if SerialDevice <> nil then state := stOpen;
          end;
        stOpen :
          begin
             res := SerialDeviceOpen (SerialDevice, 115200, SERIAL_DATA_8BIT, SERIAL_STOP_1BIT, SERIAL_PARITY_NONE, SERIAL_FLOW_NONE, 0, 0);
            //res := SerialDeviceOpen (SerialDevice, 9600, SERIAL_DATA_8BIT, SERIAL_STOP_1BIT, SERIAL_PARITY_NONE, SERIAL_FLOW_NONE, 0, 0);
            if res = ERROR_SUCCESS then state := stOpened
            else if res = ERROR_INVALID_PARAMETER then state := stFind;
          end;
        stOpened :
          begin
            res := SerialDeviceRead (SerialDevice, @Character, SizeOf (Character), SERIAL_READ_NON_BLOCK, Count);
            if (res = ERROR_SUCCESS) and (Count > 0) then  // non blocking so count may be 0
              begin
                if Character = #13 then
                  begin
                    ConsoleWindowWriteLn (WindowHandle, 'Received a line: ' + Characters);
                    Characters := 'You said ' + Characters + #13#10;
                    SerialDeviceWrite (SerialDevice, PChar (Characters),Length (Characters), SERIAL_WRITE_NONE, Count);
                    Characters:='';
                  end
                else
                  Characters := Characters + Character;
              end
            else if res = ERROR_INVALID_PARAMETER then
              begin
                if SerialDevice <> nil then SerialDeviceClose (SerialDevice);
                state := stFind;
              end;
          end;
        end; // case
      sleep (100);
    end; // true;
  { Halt the thread if we exit the loop }
  ThreadHalt (0);
end.

