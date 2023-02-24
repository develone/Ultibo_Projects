program BLE_UARTTest;

{$mode objfpc}{$H+}

{ Raspberry Pi 3 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

{$define use_tftp}

uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  uLog,
  Devices,
  {$ifdef use_tftp}
  uTFTP, Winsock2,
  {$endif}
  uBT, uBT_UART, uBLE,
  Ultibo, Console
  { Add additional units here };

var
  Console1, Console2, Console3 : TWindowHandle;
{$ifdef use_tftp}
  IPAddress : string;
{$endif}
  ch : char;
  s : string;
  BT : PBTDevice;

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

function GetFirstBT : PBTDevice;
begin
  Result := BTDeviceFind (0);
end;

function DeviceEnum (Device : PDevice; Data : Pointer) : LongWord;
begin
  Log1 ('Device ' + Device^.DeviceName + ' ' + DeviceClassToString (Device^.DeviceClass) + '  ' + Device^.DeviceDescription);
  Result := 0;
end;

procedure LEEvent (BT : PBTDevice; SubEvent : byte; Params : array of byte);
begin
  Log ('LE Event.');
end;

procedure MarkerEvent (BT : PBTDevice; Code : Word);
begin
  case code of
    FIRMWARE_START : log ('Firmware Start.');
    FIRMWARE_END   : log ('Firmware End.');
    DELAY_50MSEC   : log ('50 ms Delay.');
    DELAY_2SEC     : log ('2 sec Delay.');
    INIT_COMPLETE  : log ('Init Complete.');
    FLUSH_PORT     : log ('Flush Port.');
    OPEN_PORT      : log ('Open Port.');
    CLOSE_PORT     : log ('Close Port.');
    end;
end;

begin
  Console1 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_LEFT, true);
  Console2 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_TOPRIGHT, false);
  Console3 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOMRIGHT, false);
  SetLogProc (@Log1);

  Log ('BT (UART) Tester.');
  Log ('');

{$ifdef use_tftp}
  IPAddress := WaitForIPComplete;
  Log2 ('TFTP : Usage tftp -i ' + IPAddress + ' put kernel7.img');
  SetOnMsg (@Msg2);
  Log2 ('');
{$endif}
  BTInit;
  BT_UARTInit;
  BT := GetFirstBT;
  if BT <> nil then
    begin
      bt^.DeviceLEEvent := @LEEvent;
      PBT_UARTDevice (BT^.Device.DeviceData)^.MarkerEvent := @MarkerEvent;
    end;
  while true do
    begin
      if ConsoleGetKey (ch, nil) then
        case (UpperCase (ch)) of
          '1' : DeviceEnumerate (DEVICE_CLASS_ANY, @DeviceEnum, nil);
          '2' : BTReadLocalName (GetFirstBT);
          '3' : BTReadLocalVersion (GetFirstBT);
          '4' : BTReadBDADDR (GetFirstBT);
          '5' : BTWriteSimplePairingMode (GetFirstBT, $01); // enable
          'I' : BTInquiry (GetFirstBT);
          'P' : BLEStartPassiveScanning (GetFirstBT);
          'A' : BLEStartActiveScanning (GetFirstBT);
          'S' : BLEStopScanning (GetFirstBT);
          'C' : ConsoleWindowClear (Console1);
          'U' :
            begin
              //s := 'Flange Modulator';
              s := 'Dychromic Ultiboator';
              Log ('Broadcasting as "' + s + '".');
              BLEClearAdvertisingData (GetFirstBT);
              BLEAddAdvertisingData (GetFirstBT, ADT_FLAGS, [$1A]);
              BLEAddAdvertisingData (GetFirstBT, ADT_COMPLETE_LOCAL_NAME, s);
              BLEStartUndirectedAdvertising (GetFirstBT);
            end;
          'X' : BLEStopAdvertising (GetFirstBT);
          'B' :
            begin
              Log ('Broadcasting as iBeacon.');
              BLEClearAdvertisingData(GetFirstBT);
              BLEAddAdvertisingData (GetFirstBT, ADT_FLAGS, [$1A]);
              BLEAddAdvertisingData (GetFirstBT, ADT_MANUFACTURER_SPECIFIC,
                    [lo (ID_APPLE), hi (ID_APPLE), // company identifier
                     $02, $15,                     // advertisement indicator
                     $63, $6F, $3F, $8F, $64, $91, $4B, $EE, $95, $F7, $D8, $CC, $64, $A8, $63, $B5, // our iBeacon proximity uuid
                     $00, $01,                     // Major
                     $00, $02, 	                   // Minor
                     $C8, $00]); 	                 // Calibrated Tx power
              BLEStartUndirectedAdvertising (GetFirstBT);
            end;
          end;
    end;
  ThreadHalt (0);
end.


