unit uBT_UART;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Serial, BCM2710, Devices, GlobalConst, GlobalTypes, Threads, uBT;
(*

Connection to BCM4343 chip

UART0      115200 BAUD, 8 DATA, 1 STOP, NO PARITY, NO FLOW CONTROL

PIN 15     SET TO INPUT
PIN 32     SET TO ALT3
PIN 33     SET TO ALT3

*)
{==============================================================================}
{Global definitions}
{--$INCLUDE GlobalDefines.inc}

{==============================================================================}
const
  {BT_UART logging}
  BT_UART_LOG_LEVEL_DEBUG          = LOG_LEVEL_DEBUG;  {BT_UART debugging messages}
  BT_UART_LOG_LEVEL_INFO           = LOG_LEVEL_INFO;   {BT_UART informational messages, such as a device being attached or detached}
  BT_UART_LOG_LEVEL_ERROR          = LOG_LEVEL_ERROR;  {BT_UART error messages}
  BT_UART_LOG_LEVEL_NONE           = LOG_LEVEL_NONE;   {No BT_UART messages}

  ny : array [boolean] of string      = ('NO', 'YES');

var
  BT_UART_DEFAULT_LOG_LEVEL : LongWord = BT_UART_LOG_LEVEL_DEBUG;
  BT_UART_LOG_ENABLED : Boolean;
//  Report_Size : Longword = 5;

const
  BT_UART_DRIVER_NAME                 = 'BT (UART) Driver'; {Name of UART BT driver}
  BT_UART_DESCRIPTION                 = 'BT (UART)';        {Description of UART BT device}

  HCI_COMMAND_PKT		                  = $01;
  HCI_ACLDATA_PKT		                  = $02;
  HCI_SCODATA_PKT		                  = $03;
  HCI_EVENT_PKT		                    = $04;
  HCI_VENDOR_PKT		                  = $ff;

  // Markers
  FIRMWARE_START                      = 100;
  FIRMWARE_END                        = 101;
  DELAY_50MSEC                        = 102;
  DELAY_2SEC                          = 103;
  INIT_COMPLETE                       = 104;
  FLUSH_PORT                          = 105;
  OPEN_PORT                           = 106;
  CLOSE_PORT                          = 107;

type

  TMarkerEvent = procedure (BT : PBTDevice; Code : Word);

  {BT_UART Device}
  PBT_UARTDevice = ^TBT_UARTDevice;

  TBT_UARTDevice = record
    BT : TBTDevice;                         // bt device workings
    UART : PSerialDevice;
    UARTName : string;
    ReadHandle : TThreadHandle;
    MarkerEvent : TMarkerEvent;
    RxBuffer : array of byte;
    EventRemaining, EventPos : LongWord;    // number of bytes still remaining of event
    EventResponse : array of byte;
  end;

procedure BT_UARTInit;

function BTOpenPort (BT : PBT_UARTDevice) : LongWord;
procedure BTClosePort (BT : PBT_UARTDevice);

implementation

uses Platform, GlobalConfig, FileSystem, uLog;

var
  BT_UARTInitialized : Boolean = false;

function BT_UARTDeviceCommand (BT : PBTDevice; Buffer : Pointer; Size : Longword; var Count : Longword) : LongWord;
var
  Cmd : array of byte;
  BTU : PBT_UARTDevice;
  op : Word;
begin
  Result := ERROR_INVALID_PARAMETER;
  BTU := PBT_UARTDevice (BT.Device.DeviceData);
  if Size < 3 then exit;
  SetLength (Cmd, Size + 1);
  Cmd[0] := HCI_COMMAND_PKT;
  Move (Buffer^, Cmd[1], Size);
  op := (Cmd[2] * $100) + Cmd[1];
  if (ogf (op) = $00) and (ocf (op) <> $00) then
    begin
      Result := ERROR_SUCCESS;
      if Assigned (BTU.MarkerEvent) then BTU.MarkerEvent (BT, ocf (op));
      case ocf (op) of
        DELAY_50MSEC :
          begin
            BT.QueueEvent.WaitFor (50);
            BT.QueueEvent.SetEvent;
          end;
        DELAY_2SEC   :
          begin
            BT.QueueEvent.WaitFor (2000);
            BT.QueueEvent.SetEvent;
          end;
        OPEN_PORT    :
          begin
            BTOpenPort (BTU);
            BT.QueueEvent.SetEvent;
          end;
        CLOSE_PORT   :
          begin
            BTClosePort (BTU);
            BT.QueueEvent.SetEvent;
          end;
        else BT.QueueEvent.SetEvent;
        end;
    end
  else
    begin
      Result := SerialDeviceWrite (BTU.UART, @Cmd[0], length (Cmd), SERIAL_WRITE_NONE, count);
      if Result <> ERROR_SUCCESS then Log ('Error writing to BT.');
    end;
end;

function BTReadExecute (Parameter : Pointer) : PtrInt;
var
  c, count, res : LongWord;
  b : byte;
  i, j, rm : integer;
  decoding : boolean;
  pkt : array of byte;
  BT : PBT_UARTDevice;
begin
  Result := ERROR_SUCCESS;
  c := 0;
  Log ('Read Execute started');
  BT := PBT_UARTDevice (Parameter);
  while True do
    begin
      if SerialDeviceRead (BT.UART, @b, 1, SERIAL_READ_NONE, c) = ERROR_SUCCESS then
        begin       // One byte was received, try to read everything that is available
          SetLength (BT.RxBuffer, length (BT.RxBuffer) + 1);
          BT.RxBuffer[high (BT.RxBuffer)] := b;
          while SerialDeviceRead (BT.UART, @b, 1, SERIAL_READ_NON_BLOCK, c) = ERROR_SUCCESS do
            begin
              SetLength (BT.RxBuffer, length (BT.RxBuffer) + 1);
              BT.RxBuffer[high (BT.RxBuffer)] := b;
            end;
          i := 0;
          decoding := true;
          while decoding do
            begin
              decoding := false;
              if (i + 2 <= high (BT.RxBuffer)) then // mimumum
                 if i + BT.RxBuffer[i + 2] + 2 <= high (BT.RxBuffer) then
                   begin
                     if BT.RxBuffer[i] = HCI_EVENT_PKT then
                       begin
                         SetLength (pkt, BT.RxBuffer[i + 2] + 2);
                         Move (BT.RxBuffer[i + 1], pkt[0], length (pkt));
                         if Assigned (BT.BT.DeviceEvent) then
                          begin
                            count := 0;
                            res := BT.BT.DeviceEvent (@BT.BT, @pkt[0], length (pkt), count);
                            if res <> ERROR_SUCCESS then
                              begin
                                log ('error device event ' + res.ToString);
                              end;
                          end;
                       end;
                     i := i + length (pkt) + 1;
                     decoding := i < high (BT.RxBuffer);
                   end;
            end; // decoding
          if i > 0 then
            begin
              rm := length (BT.RxBuffer) - i;
//              Log ('Remaining ' + IntToStr (rm));
              if rm > 0 then                                 // replace with move
                for j := 0 to rm - 1 do BT.RxBuffer[j] := BT.RxBuffer[j + i];
              SetLength (BT.RxBuffer, rm);
            end;
        end;
    end;
  Log ('Read Execute terminated');
end;

procedure BTAddMarker (BT : PBTDevice; Code : word);
begin
  BTAddCommand (BT, $000, code, []);
end;

procedure BTLoadFirmware (BT : PBTDevice; fn : string);
var
  hdr : array [0 .. 2] of byte;
  Params : array of byte;
  n, len : integer;
  Op : Word;
begin               // firmware file BCM43430A1.hcd under \lib\firmware
//  Log ('Loading Firmware file ' + fn);
  FWHandle := FSFileOpen (fn, fmOpenRead);
  if FWHandle > 0 then
    begin
      BTAddMarker (BT, FIRMWARE_START);
      BTAddCommand (BT, OGF_VENDOR, $2e, []);
      BTAddMarker (BT, DELAY_50MSEC);
      n := FSFileRead (FWHandle, hdr, 3);
      while (n = 3) do
        begin
          Op := (hdr[1] * $100) + hdr[0];
          len := hdr[2];
          SetLength (Params, len);
          n := FSFileRead (FWHandle, Params[0], len);
          if (len <> n) then Log ('Data mismatch.');
          BTAddCommand (BT, Op, Params);
          n := FSFileRead (FWHandle, hdr, 3);
        end;
      FSFileClose (FWHandle);
      BTAddMarker (BT, FIRMWARE_END);
      BTAddMarker (BT, CLOSE_PORT);
      BTAddMarker (BT, DELAY_2SEC);
      BTAddMarker (BT, OPEN_PORT);
    end
  else
    Log ('Error loading Firmware file ' + fn);
end;

procedure BT_UARTInit;
var
  BT : PBT_UARTDevice;
begin
  if BT_UARTInitialized then Exit;
  BT := PBT_UARTDevice (BTDeviceCreateEx (SizeOf (TBT_UARTDevice)));
  if BT = nil then exit;
  BT.BT.Device.DeviceBus := DEVICE_BUS_SERIAL;
  BT.BT.Device.DeviceType := BT_TYPE_UART; // as opposed to BT_TYPE_USB or others
  BT.BT.Device.DeviceData := BT;
  BT.BT.DeviceCommand := BT_UARTDeviceCommand;
  BT.BT.DeviceEvent := BTDeviceEvent;
  BT.MarkerEvent := nil;
  BT.BT.Device.DeviceDescription := BT_UART_DESCRIPTION;
  BT.BT.BTId := DEVICE_ID_ANY;
  if BTDeviceRegister (@BT.BT) <> ERROR_SUCCESS then
    begin
      BTDeviceDestroy (@BT.BT);
      exit;
    end;
  if BTDeviceSetState (@BT.BT, BT_STATE_ATTACHED) <> ERROR_SUCCESS then exit;
  BT.UARTName := BCM2710_UART0_DESCRIPTION;
  BTOpenPort (BT);
  BTLoadFirmware (@BT.BT, 'BCM43430A1.hcd');
  BT_UARTInitialized := True;
end;

function BTOpenPort (BT : PBT_UARTDevice) : LongWord;
var
  res : LongWord;
begin
  Result := ERROR_INVALID_PARAMETER;
  Log ('Opening Port ' + BT.UARTName);
  BT.UART := SerialDeviceFindByDescription (BT.UARTName);
  if BT.UART = nil then exit;
  //Log ('UART Found OK.');
  res := SerialDeviceOpen (BT.UART, 115200, SERIAL_DATA_8BIT, SERIAL_STOP_1BIT, SERIAL_PARITY_NONE, SERIAL_FLOW_NONE, 0, 0);
  if res = ERROR_SUCCESS then
    begin
      Log ('UART Opened OK.');
      if BT.UARTName = BCM2710_UART0_DESCRIPTION then // main uart
        begin      // redirect serial lines to bluetooth device
          GPIOFunctionSelect (GPIO_PIN_15, GPIO_FUNCTION_IN);
          GPIOFunctionSelect (GPIO_PIN_32, GPIO_FUNCTION_ALT3);     // TXD0
          GPIOFunctionSelect (GPIO_PIN_33, GPIO_FUNCTION_ALT3);     // RXD0
        end;
      BT.ReadHandle := BeginThread (@BTReadExecute, BT, BT.ReadHandle, THREAD_STACK_DEFAULT_SIZE);
      if BT.ReadHandle = INVALID_HANDLE_VALUE then exit;
    end;
   Result := ERROR_SUCCESS;
end;

procedure BTClosePort (BT : PBT_UARTDevice);
begin
  Log ('Closing UART0');
  if BT.ReadHandle <> INVALID_HANDLE_VALUE then KillThread (BT.ReadHandle);
  BT.ReadHandle := INVALID_HANDLE_VALUE;
  if BT.UART <> nil then SerialDeviceClose (BT.UART);
  BT.UART := nil;
end;

initialization

end.

