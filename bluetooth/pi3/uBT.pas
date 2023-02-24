unit uBT;

{$mode delphi}{$H+}
// { $define show_data}

interface

uses
  Classes, SysUtils, GlobalConst, GlobalTypes, uLog, Devices, SyncObjs;
(*


OCF        OPCODE GROUP FIELD (HIGHER 6 BITS OF OP CODE)

  OGF_LINK_CONTROL	   0x01 COMMANDS TO CONTROL CONNECTIONS TO OTHER BT DEVICES
  OGF_LINK_POLICY	     0x02
  OGF_HOST_CONTROL 	   0x03 COMMANDS TO ACCESS AND CONTROL HARDWARE
  OGF_INFORMATIONAL	   0x04 COMMANDS THAT PROVIDE INFO ON AND CAPABLILITIES OF HARDWARE
  OGF_STATUS           0x05 COMMANDS THAT ACCESS STATE OF LINK MANAGER
  OGF_LE_CONTROL       0x08 COMMANDS THAT ACCESS THE LOW ENEGY (LE) FEATURES
  OGF_VENDOR           0x3F VENDOR SPECIFIC COMMANDS

  OP CODE = (OGF SHL 10) OR OCF

COMMAND FORMAT

    HCI_COMMAND_PKT    0x01  (UART ONLY)
    LO (OP CODE)
    HI (OP CODE)
    PARAM LENGTH
    PARAMS....
*)

const
  BT_NAME_PREFIX                = 'Bluetooth';

  BT_TYPE_NONE                  = 0;
  BT_TYPE_USB                   = 1;
  BT_TYPE_UART                  = 2;
  BT_TYPE_OTHER                 = 3;
  BT_TYPE_MAX                   = 3;

 {BT Type Names}
 BT_TYPE_NAMES : array [BT_TYPE_NONE .. BT_TYPE_MAX] of string = (
  'BT_TYPE_NONE', 'BT_TYPE_USB', 'BT_TYPE_UART', 'BT_TYPE_OTHER');

  BT_STATE_DETACHED             = 0;
  BT_STATE_DETACHING            = 1;
  BT_STATE_ATTACHING            = 2;
  BT_STATE_ATTACHED             = 3;

  BT_STATE_MAX                  = 3;

  {BT State Names}
  BT_STATE_NAMES : array [BT_STATE_DETACHED .. BT_STATE_MAX] of string = (
                      'BT_STATE_DETACHED', 'BT_STATE_DETACHING',
                      'BT_STATE_ATTACHING', 'BT_STATE_ATTACHED');


  BT_LOG_LEVEL_DEBUG            = LOG_LEVEL_DEBUG;  {BT debugging messages}
  BT_LOG_LEVEL_INFO             = LOG_LEVEL_INFO;   {BT informational messages, such as a device being attached or detached}
  BT_LOG_LEVEL_ERROR            = LOG_LEVEL_ERROR;  {BT error messages}
  BT_LOG_LEVEL_NONE             = LOG_LEVEL_NONE;   {No BT messages}

  // Markers
  FIRMWARE_START                = 100;
  FIRMWARE_END                  = 101;
  DELAY_50MSEC                  = 102;
  DELAY_2SEC                    = 103;
  INIT_COMPLETE                 = 104;
  FLUSH_PORT                    = 105;
  OPEN_PORT                     = 106;
  CLOSE_PORT                    = 107;

  // from bluez-5.32 hciattach_bcm43xx.c
  CC_MIN_SIZE                   = 7;

  // Link Layer specification Section 4.4.3, Core 4.1 page 2535
  LL_SCAN_WINDOW_MAX		        = 10240000;	// 10.24s
  LL_SCAN_INTERVAL_MAX	        = 10240000;	// 10.24s

  SCAN_WINDOW		                =	200000;
  SCAN_INTERVAL		              =	500000;
  SCAN_DURATION		              =	10000000;	// 10s

  BDADDR_LEN			              = 6;

  BDADDR_TYPE_PUBLIC		        = 0;
  BDADDR_TYPE_RANDOM		        = 1;

  BT_MAX_HCI_EVENT_SIZE	        = 257;
  BT_MAX_HCI_COMMAND_SIZE	      = 258;
  BT_MAX_DATA_SIZE	            = BT_MAX_HCI_COMMAND_SIZE;

  BT_BD_ADDR_SIZE		            = 6;
  BT_CLASS_SIZE		              = 3;
  BT_NAME_SIZE		              = 248;

  OGF_MARKER                    = $00;
  OGF_LINK_CONTROL	            =	$01;
  OGF_LINK_POLICY	              =	$02;
  OGF_HOST_CONTROL 	            = $03;  // aka controller and baseband commands
  OGF_INFORMATIONAL	            =	$04;
  OGF_LE_CONTROL                = $08;
  OGF_VENDOR                    = $3f;

  //OP_CODE_WRITE_CLASS_OF_DEVICE	= OGF_HCI_CONTROL_BASEBAND or $024;

  INQUIRY_LAP_GIAC		          = $9E8B33;	// General Inquiry Access Code

  INQUIRY_LENGTH_MIN		        = $01;		// 1.28s
  INQUIRY_LENGTH_MAX		        = $30;		// 61.44s
  //#define INQUIRY_LENGTH(secs)		(((secs) * 100 + 64) / 128)
  INQUIRY_NUM_RESPONSES_UNLIMITED	= $00;


type
  PBTDevice = ^TBTDevice;

  TBTMarkerEvent = procedure (BT : PBTDevice; no : integer);
  TBTDeviceLEEvent = procedure (BT : PBTDevice; SubEvent : byte; Params : array of byte);

  TBTDeviceEvent = function (BT : PBTDevice; Buffer : Pointer; Size  : LongWord; var Count : LongWord) : LongWord;
  TBTDeviceCommand = function (BT : PBTDevice; Buffer : Pointer; Size : Longword; var Count : Longword) : LongWord;

  TBTEnumerate = function (BT : PBTDevice; Data : Pointer) : LongWord;
  TBTNotification = function (Device : PDevice; Data:Pointer; Notification : LongWord) : LongWord;

  TBDAddr = array [0 .. BDADDR_LEN - 1] of byte;
  TBDKey = array [0 .. 15] of byte;
  TLEData = array [0 .. 30] of byte;
  TLEKey = array [0 .. 15] of byte;

  PQueueItem = ^TQueueItem;
  TQueueItem = record
    OpCode : Word;
    Params : array of byte;
    Prev, Next : PQueueItem;
  end;


  TBTDevice = record
    Device : TDevice;                    // The Device entry for this BT device
    BTId : LongWord;                     // Unique Id of this BT device in the BT table
    BTState : LongWord;                  // device state (eg BT_STATE_ATTACHED)
    DeviceEvent : TBTDeviceEvent;
    DeviceCommand : TBTDeviceCommand;
    DeviceLEEvent : TBTDeviceLEEvent;
    First, Last : PQueueItem;            // command queue for device
    Queue : TMailslotHandle;
    QueueHandle : TThreadHandle;
    QueueEvent : TEvent;
    ChipName : string;
    Ver : byte;
    Rev : Word;
    BDAddr : TBDAddr; //  = ($b8, $27, $e8, $cc, $72, $27);
    AdData : array of byte;

    QueueItem : PQueueItem;          // queue vars
    Cmd : array of byte;
    res, count : LongWord;

    Lock : TMutexHandle;                 // lock
    //Buffer:TBLEBuffer;                 {input buffer}
    Prev : PBTDevice;                    // Previous entry in BT table
    Next : PBTDevice;                    // Next entry in BT table
  end;

// general BT device / driver routines

procedure BTInit;
function BTDeviceSetState (BT:PBTDevice; State : LongWord) : LongWord;

function BTDeviceCreate : PBTDevice;
function BTDeviceCreateEx (Size : LongWord) : PBTDevice;
function BTDeviceDestroy (BT : PBTDevice) : LongWord;

function BTDeviceRegister (BT:PBTDevice) : LongWord;
function BTDeviceDeregister (BT:PBTDevice) : LongWord;

function BTDeviceFind (BTId : LongWord) : PBTDevice;
function BTDeviceFindByName (const Name : string) : PBTDevice; inline;
function BTDeviceFindByDescription (const Description : string) : PBTDevice; inline;
function BTDeviceEnumerate (Callback : TBTEnumerate; Data : Pointer) : LongWord;
function BTDeviceNotification (BT : PBTDevice; Callback : TBTNotification; Data : Pointer; Notification, Flags : LongWord) : LongWord;

function BTDeviceEvent (BT : PBTDevice; Buffer : Pointer; Size  : LongWord; var Count : LongWord) : LongWord;

function BTGetCount : LongWord; inline;

function BTDeviceCheck (BT:PBTDevice) : PBTDevice;

function BTDeviceTypeToString (BTType : LongWord): string;
function BTDeviceStateToString (BTState : LongWord): string;

function BTDeviceStateToNotification (State : LongWord) : LongWord;

procedure BTLog (Level : LongWord; BT : PBTDevice; const AText : string);
procedure BTLogInfo (BT : PBTDevice; const AText : string);
procedure BTLogError (BT : PBTDevice; const AText : string);
procedure BTLogDebug (BT : PBTDevice; const AText : string);

// add commands to queue
procedure BTAddCommand (BT : PBTDevice; OGF : byte; OCF : Word; Params : array of byte); overload;
procedure BTAddCommand (BT : PBTDevice; OpCode : Word; Params : array of byte); overload;

function ogf (Op : Word) : byte;
function ocf (Op : Word) : Word;
function OGFToStr (ogf : byte) : string;
function BDAddrToStr (Addr : TBDAddr) : string; overload;
function BDAddrToStr (Buffer : pointer) : string; overload;
function ErrToStr (Code : byte) : string;
procedure BTNoOP (BT : PBTDevice);
procedure BTAddMarker (BT : PBTDevice; Marker : Word);

// HCI Commands
procedure BTResetChip (BT : PBTDevice);
procedure BTReadLocalName (BT : PBTDevice);
procedure BTReadScanEnable (BT : PBTDevice);
procedure BTWriteScanEnable (BT : PBTDevice; Enable : byte);

// Link Control
procedure BTInquiry (BT : PBTDevice);
procedure BTInquiryCancel (BT : PBTDevice);
procedure BTLinkKeyRequestReply (BT : PBTDevice; Addr : TBDAddr; Key : TBDKey);
procedure BTLinkKeyRequestNegativeReply (BT : PBTDevice; Addr : TBDAddr);
procedure BTLinkCodeRequestReply (BT : PBTDevice; Addr : TBDAddr; Code : string);
procedure BTAuthenticationRequested (BT : PBTDevice; Handle : Word);

// Controller / Baseband
procedure BTWriteSimplePairingMode (BT : PBTDevice; Mode : byte);
procedure BTReadLocalOOBData (BT : PBTDevice);

// Informational Parameters
procedure BTReadLocalVersion (BT : PBTDevice);
procedure BTReadLocalSupportedCommands (BT : PBTDevice);
procedure BTReadLocalSupportedFeatures (BT : PBTDevice);
procedure BTReadBDADDR (BT : PBTDevice);

// LE
procedure BTSetLEEventMask (BT : PBTDevice; Mask : QWord);
procedure BTReadLEBufferSize (BT : PBTDevice);
procedure BTReadLESupportedFeatures (BT : PBTDevice);
procedure BTSetLERandomAddress (BT : PBTDevice; Addr : TBDAddr);
procedure BTSetLEAdvertisingParameters (BT : PBTDevice; MinInterval, MaxInterval : Word;
                                     Type_ : byte;
                                     OwnAddressType, PeerAddressType : byte;
                                     PeerAddr : TBDAddr;
                                     ChannelMap, FilterPolicy : byte);
procedure BTReadLEAdvertisingChannelTxPower (BT : PBTDevice);
procedure BTSetLEAdvertisingData (BT : PBTDevice; Data : array of byte);
procedure BTSetLEScanResponseData (BT : PBTDevice; Data : array of byte);
procedure BTSetLEAdvertisingEnable (BT : PBTDevice; State : boolean);
procedure BTSetLEScanParameters (BT : PBTDevice; Type_ : byte; Interval, Window : Word;
                               OwnAddressType, FilterPolicy : byte);
procedure BTSetLEScanEnable (BT : PBTDevice; State, Duplicates : boolean);
procedure BTLERand (BT : PBTDevice);
procedure BTStartLEEncryption (BT : PBTDevice; Handle : Word; Rand : QWord; Diversifier : Word; Key : TLEKey);

var
  BT_LOG_ENABLED : boolean;
  BT_DEFAULT_LOG_LEVEL : LongWord = BT_LOG_LEVEL_DEBUG;
  FWHandle : integer; // firmware file handle

implementation

uses Platform, GlobalConfig, Threads, FileSystem;

var
  RxBuffer : array of byte;
  BTInitialized : Boolean = false;
  BTTable : PBTDevice;
  BTTableLock : TCriticalSectionHandle = INVALID_HANDLE_VALUE;
  BTTableCount : LongWord;

const
  ADV_IND                     = $00; // Connectable undirected advertising (default)
  ADV_DIRECT_IND_HI           = $01; // Connectable high duty cycle directed advertising
  ADV_SCAN_IND                = $02; // Scannable undirected advertising
  ADV_NONCONN_IND             = $03; // Non connectable undirected advertising
  ADV_DIRECT_IND_LO           = $04; // Connectable low duty cycle directed advertising

function BTQueueHandler (Parameter : Pointer) : PtrInt;
var
  BT : PBTDevice;
//  i : integer;
//  s : string;
begin
  Result := 0;
  BT := PBTDevice (Parameter);
  while true do
    begin
      BT.QueueEvent.ResetEvent;
      BT.QueueItem := PQueueItem (MailslotReceive (BT.Queue));
      if BT.QueueItem <> nil then
        begin
          SetLength (BT.Cmd, length (BT.QueueItem.Params) + 3);
          BT.Cmd[0] := lo (BT.QueueItem.OpCode);          // little endian so lowest sent first
          BT.Cmd[1] := hi (BT.QueueItem.OpCode);
          BT.Cmd[2] := length (BT.QueueItem^.Params);
          Move (BT.QueueItem^.Params[0], BT.Cmd[3], length (BT.QueueItem.Params));
//              for i := 0 to length (BT.QueueItem^.Params) - 1 do Cmd[4 + i] := BT.QueueItem^.Params[i];
          BT.Count := 0;
(*          if ogf (BT.QueueItem^.OpCode) <> OGF_VENDOR then
            begin
              s := '';
              for i := 0 to length (BT.Cmd) - 1 do s := s + ' ' + BT.Cmd[i].ToHexString (2);
              Log ('--> ' + s);
            end; *)
          BT.Res := ERROR_WRITE_FAULT;
          if Assigned (BT.DeviceCommand) then
            BT.Res := BT.DeviceCommand (BT, @BT.Cmd[0], length (BT.Cmd), BT.Count);
          if BT.Res = ERROR_SUCCESS then
            begin
             // x :=  the wait for here is clashing with the wait for on the marker
              if BT.QueueEvent.WaitFor (10000) <> wrSignaled then
                Log ('Timeout waiting for BT Response.'); // should send nop ???
            end
          else
            Log ('Error writing to BT.');
        end;
      SetLength (BT.QueueItem.Params, 0);
      Dispose (BT.QueueItem);
    end;
end;

function BTDeviceEvent (BT : PBTDevice; Buffer : Pointer; Size  : LongWord; var Count : LongWord) : LongWord;
var
  len, num : byte;
  op : Word;
  pt : word;
  nr, ofs, se : byte; // number of responses
  i, j : integer;
  b, prm : array of byte;
  s : string;
  h : Word;
  Key : TBDKey;
  Addr : TBDAddr;

  procedure ListAD (ad : array of byte);
  begin
    if length (ad) = 0 then exit;
    case ad[0] of
      $01 : // flags
        if length (ad) = 2 then
          Log ('Flags : ' + ad[1].ToHexString (2));
      $ff : // manufacturer specific
        begin
          Log ('Manufacturer Specific');
        end;
      end;
  end;

begin
  Result := ERROR_INVALID_PARAMETER;
  if size < 2 then exit;
  b := Buffer;
  len := b[1];
  num := 0;
//  Log ('Cmd ' + b[0].ToHexString (2) + ' Len ' + len.ToString + ' Size ' + Size.ToString);
  if len + 2 <> Size then exit;
  case b[0] of             // event code
    $01 :   // inquiry complete
      begin
        if len = 1 then Log ('Inquiry Complete : ' + ErrToStr (b[2]));
      end;
    $02 : // inquiry result event
      begin
        log ('Inquiry Result.');
        if len > 1 then
           begin
             nr := b[2];
             if (nr * 14) + 1 = len then
                begin
                  Log ('Inquiry result nos ' + b[2].tostring + ' Len ' + len.ToString);
                  for i := 1 to nr do
                    begin
                      ofs := ((i - 1) * 14) + 3;
                      s := '  Controller ' + i.ToString + '  BD Addr : ';
                      for j := 5 downto 0 do s := s + b[j + ofs].ToHexString (2) + ':';
                      delete (s, length (s), 1);
                      s := s + ' PSR Mode ' + b[6 + ofs].ToHexString (2);
                      j := b[9 + ofs] + b[10 + ofs] * $100 + b[11 + ofs] * $10000;
                      s := s + ' Class ' + j.ToHexString (6);
                      Log (s);
                (*      002540 // keyboard
                        (13-23)
                        0000 0000 0010 0101 0100 0000
                          limited discoverable mode
                          keyboard - keyboard
                      *)
                      if j = $2540 then // keyboard
                        begin
                          Log ('Is Keyboard');
                          pt := $0000;
                          SetLength (prm, 13);
                          Move (b[ofs], prm[0], 6);           // addr
                          prm[6] := lo (pt);
                          prm[7] := hi (pt);
                          prm[8] := b[6 + ofs];               // r1 page scan repetition mode
                          prm[9] := $00;                      // reserved
                          prm[10] := b[12 + ofs];             // clock offset
                          prm[11] := b[13 + ofs];
                          prm[12] := $01;                     // allow role switch
                          BTAddCommand (BT, OGF_LINK_CONTROL, $05, prm);
                        end;
                    end;
                end;
           end;
      end;
    $03 : // connection complete
      begin
//        Log ('len ' + len.ToString);
        if len = 11 then
          begin
            Log ('Connection Complete : ' + ErrToStr (b[2]) + '.');
            Log ('Handle ' + IntToHex (b[3] + b[4] * $100, 4));
            Log ('BD Addr ' + BDAddrToStr (@b[5]));
            Log ('Link Type ' + b[11].ToHexString(2));
            Log ('Encryption ' + b[12].ToHexString(2));
            h := b[3] + b[4] * $100;
            BTAuthenticationRequested (BT, h);
          end;
      end;
    $05 :  // connection disconnect
      begin
        Log ('Connection Disconnected.');
        Log ('Status ' + ErrToStr (b[2]) + '.');
        Log ('Handle ' + IntToHex (b[3] + b[4] * $100, 4));
        Log ('Reason ' + ErrToStr (b[5]) + '.');
      end;
    $06 :  // authentication complete
      begin
        Log ('Authentication Complete.');
        Log ('Status ' + ErrToStr (b[2]) + '.');
        Log ('Handle ' + IntToHex (b[3] + b[4] * $100, 4));
      end;
    $0e :  // command complete
      begin
        num := b[2];          // num packets controller can accept
        op := b[4] * $100 + b[3];
        //Log ('OGF ' + inttohex (ogf (op), 2) + ' OCF ' + inttohex (ocf (op), 3) + ' OP Code ' + inttohex (op, 4) + ' Num ' + num.ToString + ' Len ' + len.ToString);
        if (len > 3) and (b[5] > 0) then Log ('Status ' + ErrToStr (b[5]));
        case op of         // b indexes changed
          $0c14 : // read name
            begin
              BT.ChipName := '';
              i := 6;
              while (i <= len + 3) and (b[i] <> $00) do
                begin
                  BT.ChipName := BT.ChipName + chr (b[i]);
                  i := i + 1;
                end;
              log ('Chip name : "' + BT.ChipName + '"');
            end;
          $1001 : // read local version
            begin
              if len = 12 then
                begin
                  BT.Ver := b[6];
                  BT.Rev := b[8] * $100 + b[7];
                  log ('Ver ' + BT.Ver.ToString + ' rev ' + BT.Rev.ToString);
                end;
            end;
          $1009 : // read bd addr
            begin
              if len = 10 then for i := 0 to 5 do BT.BDAddr[i] := b[6 + i];
              log ('Address : ' + BDAddrToStr (BT.BDAddr));
            end;
          $2007 : // read le channel tx power
            begin
              if len = 5 then Log ('Tx Power ' + b[7].ToString);
            end;
          else
            begin
              if (len > 3) then Log (OGFToStr (ogf (op)) + ' ' + IntToHex (ocf (op), 3) + ' Completed - ' + ErrToStr (b[5]));
            end;
          end;  // case op
        end;  // command complete
      $0f : // command status
        begin
      //    Log ('Command Status');
          if (len = 4) then
            begin
              num := b[3];
              op := b[5] * $100 + b[4];
              Log (OGFToStr (ogf (op)) + ' ' + IntToHex (ocf (op), 3) + ' Status - ' + ErrToStr (b[2]));
            end;
        end;
      $1b : // max slots change event
        begin
          if (len = 3) then
            begin
              s := 'Max Slots Changed - Handle ' + IntToHex (b[3] + b[4] * $100, 4) + ' Slots ' + b[5].ToString;
              Log (s);
            end;
        end;
      $17 :
        begin
          Log ('Link Key Request BD Addr ' + BDAddrToStr (@b[2]));
          FillChar (Key, 16, 0);
          Addr[5] := b[2];
          Addr[4] := b[3];
          Addr[3] := b[4];
          Addr[2] := b[5];
          Addr[1] := b[6];
          Addr[0] := b[7];
          // BTLinkKeyRequestReply (BT, Addr, Key);
          BTLinkKeyRequestNegativeReply (BT, Addr);
        end;
      $3e : // le meta event
        begin
          if (len > 2) then
            begin
              se := b[3];
              if Assigned (BT.DeviceLEEvent) then
                begin
                  SetLength (prm, len - 1);
                  Move (b[4], prm[0], len - 1);
                  BT.DeviceLEEvent (BT, se, prm);
                end;
            end; // len <=
        end   // case
      else Log ('Unknown command ' + b[0].ToString);
     end;
  if num > 0 then BT.QueueEvent.SetEvent;
  Result := ERROR_SUCCESS;
end;

procedure BTInit;
begin
  if BTInitialized then exit;
  BT_LOG_ENABLED := (BT_DEFAULT_LOG_LEVEL <> BT_LOG_LEVEL_NONE);
  BTTable := nil;
  BTTableLock := CriticalSectionCreate;
  BTTableCount := 0;
  if BTTableLock = INVALID_HANDLE_VALUE then
    begin
      if BT_LOG_ENABLED then BTLogError (nil, 'Failed to create BT table lock.');
    end;
   BTInitialized := true;
end;

function BTDeviceSetState (BT : PBTDevice; State : LongWord) : LongWord;
begin
  Result := ERROR_CAN_NOT_COMPLETE;
  if BT = nil then exit;
  if BT.Device.Signature <> DEVICE_SIGNATURE then exit;
  if State > BT_STATE_ATTACHED then exit;
  if BT.BTState = State then
    begin
      Result := ERROR_SUCCESS;
    end
  else
    begin
      if MutexLock (BT.Lock) = ERROR_SUCCESS then
        begin
          try
            BT.BTState := State;
            NotifierNotify (@BT.Device, BTDeviceStateToNotification (State));
            Result := ERROR_SUCCESS;
          finally
            MutexUnlock (BT.Lock);
            end;
        end
      else
        begin
          Result := ERROR_CAN_NOT_COMPLETE;
        end;
    end;
end;

function BTDeviceCreate : PBTDevice;
begin
  Result := BTDeviceCreateEx (SizeOf (TBTDevice));
end;

function BTDeviceCreateEx (Size:LongWord) : PBTDevice;
begin
  log ('creating');
  Result := nil;
  if Size < SizeOf (TBTDevice) then exit;
  Result := PBTDevice (DeviceCreateEx (Size));
  if Result = nil then exit;
  Result.Device.DeviceBus := DEVICE_BUS_NONE;
  Result.Device.DeviceType := BT_TYPE_NONE;
  Result.Device.DeviceFlags := 0;
  Result.Device.DeviceData := nil;
  Result.BTId := DEVICE_ID_ANY;
  Result.DeviceEvent := nil;
  Result.DeviceCommand := nil;
  Result.Lock := INVALID_HANDLE_VALUE;
  Result.Lock := MutexCreate;
  if Result.Lock = INVALID_HANDLE_VALUE then
    begin
      if BT_LOG_ENABLED then BTLogError (nil, 'Failed to create lock for BT.');
      BTDeviceDestroy (Result);
      Result := nil;
      exit;
    end;
  Result.Queue := MailSlotCreate (1024);
  Result.QueueEvent := TEvent.Create (nil, true, false, '');
  Result.QueueHandle := BeginThread (@BTQueueHandler, Result, Result.QueueHandle, THREAD_STACK_DEFAULT_SIZE);
  log ('created');
end;

function BTDeviceDestroy (BT : PBTDevice) : LongWord;
begin
  log ('destroying');
  Result := ERROR_INVALID_PARAMETER;
  if BT = nil then exit;
  if BT.Device.Signature <> DEVICE_SIGNATURE then exit;
  Result := ERROR_IN_USE;
  if BTDeviceCheck (BT) = BT then exit;
  if BT.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then exit;
  if BT.Lock <> INVALID_HANDLE_VALUE then
    begin
      MutexDestroy (BT.Lock);
    end;
  KillThread (BT.QueueHandle);
  MailSlotDestroy (BT.Queue);
  BT.QueueEvent.Free;
  Result := DeviceDestroy (@BT.Device);
  log ('destroyed');
end;

function BTDeviceRegister (BT : PBTDevice) : LongWord;
var
  BTId : LongWord;
begin
  Log ('registering');
  Result := ERROR_INVALID_PARAMETER;
  if BT = nil then exit;
  if BT.BTId <> DEVICE_ID_ANY then exit;
  if BT.Device.Signature <> DEVICE_SIGNATURE then exit;
  Result := ERROR_ALREADY_EXISTS;
   if BTDeviceCheck (BT) = BT then exit;
  if BT.Device.DeviceState <> DEVICE_STATE_UNREGISTERED then exit;
  if CriticalSectionLock (BTTableLock) = ERROR_SUCCESS then
    begin
      try
        BTId := 0;
        while BTDeviceFind (BTId) <> nil do
          begin
            Inc (BTId);
          end;
        BT.BTId := BTId;
        BT.Device.DeviceName := BT_NAME_PREFIX + IntToStr (BT.BTId);
        BT.Device.DeviceClass := DEVICE_CLASS_BLUETOOTH;
        Result := DeviceRegister (@BT.Device);
        if Result <> ERROR_SUCCESS then
          begin
            BT.BTId := DEVICE_ID_ANY;
            exit;
          end;
        if BTTable = nil then
          begin
            BTTable := BT;
          end
        else
          begin
            BT.Next := BTTable;
            BTTable.Prev := BT;
            BTTable := BT;
          end;
        Inc (BTTableCount);
        Result := ERROR_SUCCESS;
      finally
        CriticalSectionUnlock (BTTableLock);
        end;
    end
  else
    begin
      Result := ERROR_CAN_NOT_COMPLETE;
    end;
  if Result = ERROR_SUCCESS then log ('registered');
end;

function BTDeviceDeregister (BT : PBTDevice) : LongWord;
var
  Prev : PBTDevice;
  Next : PBTDevice;
begin
  log ('deregistering');
  Result := ERROR_INVALID_PARAMETER;
  if BT = nil then exit;
  if BT.BTId = DEVICE_ID_ANY then exit;
  if BT.Device.Signature <> DEVICE_SIGNATURE then exit;
  Result := ERROR_NOT_FOUND;
  if BTDeviceCheck (BT) <> BT then exit;
  if BT.Device.DeviceState <> DEVICE_STATE_REGISTERED then exit;
  if CriticalSectionLock (BTTableLock) = ERROR_SUCCESS then
    begin
      try
        Result:=DeviceDeregister (@BT.Device);
        if Result <> ERROR_SUCCESS then exit;
        Prev := BT.Prev;
        Next := BT.Next;
        if Prev = nil then
          begin
            BTTable := Next;
            if Next <> nil then
              begin
                Next.Prev := nil;
              end;
          end
        else
          begin
            Prev.Next := Next;
            if Next <> nil then
              begin
                Next.Prev := Prev;
              end;
          end;
        Dec (BTTableCount);
        BT.BTId := DEVICE_ID_ANY;
        Result := ERROR_SUCCESS;
      finally
        CriticalSectionUnlock (BTTableLock);
        end;
    end
 else
   begin
     Result := ERROR_CAN_NOT_COMPLETE;
   end;
  if Result = ERROR_SUCCESS then log ('deregistered');
end;

function BTDeviceFind (BTId:LongWord) : PBTDevice;
var
  BT : PBTDevice;
begin
  Result := nil;
  if BTId = DEVICE_ID_ANY then exit;
  if CriticalSectionLock (BTTableLock) = ERROR_SUCCESS then
    begin
      try
        BT := BTTable;
        while BT <> nil do
          begin
            if BT.Device.DeviceState = DEVICE_STATE_REGISTERED then
              begin
                if BT.BTId = BTId then
                  begin
                    Result := BT;
                    exit;
                  end;
              end;
            BT := BT.Next;
          end;
      finally
        CriticalSectionUnlock (BTTableLock);
        end;
    end;
end;

function BTDeviceFindByName (const Name : string) : PBTDevice; inline;
begin
  Result := PBTDevice (DeviceFindByName (Name));
end;

function BTDeviceFindByDescription (const Description : String) : PBTDevice; inline;
begin
 Result := PBTDevice (DeviceFindByDescription (Description));
end;

function BTDeviceEnumerate (Callback : TBTEnumerate; Data : Pointer) : LongWord;
var
  BT : PBTDevice;
begin
  Result := ERROR_INVALID_PARAMETER;
  if not Assigned (Callback) then exit;
  if CriticalSectionLock (BTTableLock) = ERROR_SUCCESS then
    begin
      try
        BT := BTTable;
        while BT <> nil do
          begin
            if BT.Device.DeviceState = DEVICE_STATE_REGISTERED then
              begin
                if Callback (BT, Data) <> ERROR_SUCCESS then exit;
              end;
            BT := BT.Next;
          end;
        Result := ERROR_SUCCESS;
      finally
        CriticalSectionUnlock (BTTableLock);
        end;
    end
  else
    begin
      Result := ERROR_CAN_NOT_COMPLETE;
    end;
end;

function BTDeviceNotification (BT : PBTDevice; Callback : TBTNotification; Data : Pointer; Notification, Flags : LongWord) : LongWord;
begin
  Result := ERROR_INVALID_PARAMETER;
  if BT = nil then
    begin
      Result := DeviceNotification (nil, DEVICE_CLASS_BLUETOOTH, Callback, Data, Notification, Flags);
    end
  else
    begin
      if BT.Device.Signature <> DEVICE_SIGNATURE then exit;
      Result := DeviceNotification (@BT.Device, DEVICE_CLASS_BLUETOOTH, Callback,Data,Notification, Flags);
    end;
end;

function BTGetCount : LongWord; inline;
begin
  Result := BTTableCount;
end;

function BTDeviceCheck (BT : PBTDevice) : PBTDevice;
var
  Current : PBTDevice;
begin
  Result := nil;
  if BT = nil then exit;
  if BT.Device.Signature <> DEVICE_SIGNATURE then exit;
  if CriticalSectionLock (BTTableLock) = ERROR_SUCCESS then
    begin
      try
        Current := BTTable;
        while Current <> nil do
          begin
            if Current = BT then
              begin
                Result := BT;
                exit;
              end;
            Current := Current.Next;
          end;
      finally
        CriticalSectionUnlock (BTTableLock);
        end;
    end;
end;

function BTDeviceTypeToString (BTType : LongWord):String;
begin
  Result := 'BT_TYPE_UNKNOWN';
  if BTType <= BT_TYPE_MAX then
    begin
      Result := BT_TYPE_NAMES[BTType];
    end;
end;

function BTDeviceStateToString (BTState : LongWord):String;
begin
  Result:='BT_STATE_UNKNOWN';
  if BTState <= BT_STATE_MAX then
    begin
      Result:=BT_STATE_NAMES[BTState];
    end;
end;

function BTDeviceStateToNotification (State : LongWord) : LongWord;
begin
  Result := DEVICE_NOTIFICATION_NONE;
  case State of
    BT_STATE_DETACHED  : Result := DEVICE_NOTIFICATION_DETACH;
    BT_STATE_DETACHING : Result := DEVICE_NOTIFICATION_DETACHING;
    BT_STATE_ATTACHING : Result := DEVICE_NOTIFICATION_ATTACHING;
    BT_STATE_ATTACHED  : Result := DEVICE_NOTIFICATION_ATTACH;
  end;
end;

procedure BTLog (Level : LongWord; BT : PBTDevice; const AText : string);
var
  WorkBuffer : string;
begin
  if Level < BT_DEFAULT_LOG_LEVEL then exit;
  WorkBuffer := '';
  if Level = BT_LOG_LEVEL_DEBUG then
    begin
      WorkBuffer := WorkBuffer + '[DEBUG] ';
    end
  else if Level = BT_LOG_LEVEL_ERROR then
    begin
      WorkBuffer := WorkBuffer + '[ERROR] ';
    end;
  WorkBuffer := WorkBuffer + 'BT: ';
  if BT <> nil then
    begin
      WorkBuffer := WorkBuffer + BT_NAME_PREFIX + IntToStr (BT.BTId) + ': ';
    end;
  LoggingOutputEx (LOGGING_FACILITY_USER, LogLevelToLoggingSeverity(Level), 'BT', WorkBuffer + AText);
end;

procedure BTLogInfo (BT : PBTDevice; const AText : string);
begin
  BTLog (BT_LOG_LEVEL_INFO, BT, AText);
end;

procedure BTLogError (BT : PBTDevice; const AText : string);
begin
  BTLog (BT_LOG_LEVEL_ERROR, BT, AText);
end;

procedure BTLogDebug (BT : PBTDevice; const AText : string);
begin
  BTLog (BT_LOG_LEVEL_DEBUG, BT, AText);
end;

function EventTypeToStr (Type_ : byte) : string;
begin
  case Type_ of
    ADV_IND           : Result := 'Connectable undirected advertising (default)';
    ADV_DIRECT_IND_HI : Result := 'Connectable high duty cycle directed advertising';
    ADV_SCAN_IND      : Result := 'Scannable undirected advertising';
    ADV_NONCONN_IND   : Result := 'Non connectable undirected advertising';
    ADV_DIRECT_IND_LO : Result := 'Connectable low duty cycle directed advertising';
    else                Result := 'Reserved for future use (' + Type_.ToHexString(2) + ')';
  end;
end;

function ogf (op : Word) : byte;
begin
  Result := (op shr 10) and $3f;
end;

function ocf (op : Word) : Word;
begin
  Result := op and $3ff;
end;

function OGFToStr (ogf : byte) : string;
begin
  case ogf of
    OGF_MARKER        : Result := 'MARKER';
    OGF_LINK_CONTROL	: Result := 'LINK CONTROL';
    OGF_LINK_POLICY	  : Result := 'LINK POLICY';
    OGF_HOST_CONTROL 	: Result := 'HOST CONTROL';
    OGF_INFORMATIONAL	: Result := 'INFORMATIONAL';
    OGF_LE_CONTROL    : Result := 'LE CONTROL';
    OGF_VENDOR        : Result := 'VENDOR';
    else                Result := 'UNKNOWN (' + IntToHex (ogf, 2) + ')';
  end;
end;

function BDAddrToStr (Buffer : pointer) : string;
var
  i : integer;
  b : PByte;
begin
  Result := '';
  b := Buffer;
  for i := 0 to 5 do
    begin
      if i = 0 then
        Result := b^.ToHexString (2)
      else
        Result := b^.ToHexString (2) + ':' + Result;
      inc (b);
    end;
end;

function BDAddrToStr (Addr : TBDAddr) : string;
var
  i : integer;
begin
  Result := '';
  for i := 0 to 5 do
    if i = 0 then
      Result := Addr[i].ToHexString (2)
    else
      Result := Result + ':' + Addr[i].ToHexString (2);
end;

procedure BTAddCommand (BT : PBTDevice; OGF : byte; OCF : Word; Params : array of byte);
begin
  BTAddCommand (BT, (OGF shl 10) or OCF, Params);
end;

procedure BTAddCommand (BT : PBTDevice; OpCode : Word; Params : array of byte);
var
  anItem : PQueueItem;
begin
  if BT = nil then exit;
  New (anItem);
  anItem^.OpCode := OpCode;
  SetLength (anItem.Params, length (Params));
  Move (Params[0], anItem.Params[0], length (Params));
  anItem^.Next := nil;
  anItem^.Prev := BT.Last;
  if BT.First = nil then BT.First := anItem;
  if BT.Last <> nil then BT.Last.Next := anItem;
  BT.Last := anItem;
  if MailSlotSend (BT.Queue, Integer (anItem)) <> ERROR_SUCCESS then
    Log ('Error adding Command to queue.');
end;

procedure BTNoOP (BT : PBTDevice);  // in spec but not liked by BCM chip
begin
  BTAddCommand (BT, $00, $00, []);
end;

procedure BTAddMarker (BT : PBTDevice; Marker : Word);
begin
  BTAddCommand (BT, OGF_MARKER, Marker and $3ff, []);
end;

// host control
procedure BTResetChip (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_HOST_CONTROL, $03, []);
end;

procedure BTReadLocalName (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_HOST_CONTROL, $14, []);
end;

procedure BTReadScanEnable (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_HOST_CONTROL, $19, []);
end;

procedure BTWriteScanEnable (BT : PBTDevice; Enable : byte);
begin
  BTAddCommand (BT, OGF_HOST_CONTROL, $14, [Enable]);
end;

// link control
procedure BTInquiry (BT : PBTDevice);
var
  Params : array of byte;
begin
  SetLength (Params, 5);
  Params[0] := $33;
  Params[1] := $8b;
  Params[2] := $9e;
  Params[3] := 1;       // inquiry length x * 1.28 secs
  Params[4] := $00;     // unlimited number of responses
  BTAddCommand (BT, OGF_LINK_CONTROL, $01, Params);
end;

procedure BTInquiryCancel (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_LINK_CONTROL, $02, []);
end;

procedure BTPeriodicInquiry (BT : PBTDevice);
var
  Params : array of byte;     // fix
begin
  SetLength (Params, 5);
  Params[0] := $33;
  Params[1] := $8b;
  Params[2] := $9e;
  Params[3] := 1;       // inquiry length x * 1.28 secs
  Params[4] := $00;     // unlimited number of responses
  BTAddCommand (BT, OGF_LINK_CONTROL, $03, Params);
end;

procedure BTExitPeriodicInquiry (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_LINK_CONTROL, $04, []);
end;

procedure BTCreateConnection (BT : PBTDevice; Addr : TBDAddr; PSR : byte; CLK : Word);
var
  Params : array of byte;
  pt : Word; // packet type
begin
  pt := $ffff;
  SetLength (Params, 13);
  Params[0] := Addr[5];
  Params[1] := Addr[4];
  Params[2] := Addr[3];
  Params[3] := Addr[2];
  Params[4] := Addr[1];
  Params[5] := Addr[0];

  Params[6] := lo (pt);
  Params[7] := hi (pt);
  Params[8] := PSR;            // r1 page scan repetition mode
  Params[9] := $00;            // reserved

  Params[10] := lo (CLK);      // clock offset
  Params[11] := hi (CLK);
  Params[12] := $01;           // allow role switch
  BTAddCommand (BT, OGF_LINK_CONTROL, $05, Params);
end;

procedure BTDisconnect (BT : PBTDevice; Handle : Word; Reason : byte);
var
  Params : array of byte;
begin
  SetLength (Params, 14);
  Params[0] := lo (Handle);
  Params[1] := hi (Handle);
  Params[2] := Reason;
  BTAddCommand (BT, OGF_LINK_CONTROL, $06, Params);
end;

procedure BTCreateConnectionCancel (BT : PBTDevice; Addr : TBDAddr);
var
  Params : array of byte;
begin
  SetLength (Params, 6);
  Params[0] := Addr[5];
  Params[1] := Addr[4];
  Params[2] := Addr[3];
  Params[3] := Addr[2];
  Params[4] := Addr[1];
  Params[5] := Addr[0];
  BTAddCommand (BT, OGF_LINK_CONTROL, $08, Params);
end;

procedure BTAcceptConnectionRequest (BT : PBTDevice; Addr : TBDAddr; Role : byte);
var
  Params : array of byte;
begin
  SetLength (Params, 7);
  Params[0] := Addr[5];
  Params[1] := Addr[4];
  Params[2] := Addr[3];
  Params[3] := Addr[2];
  Params[4] := Addr[1];
  Params[5] := Addr[0];
  Params[6] := Role;
  BTAddCommand (BT, OGF_LINK_CONTROL, $09, Params);
end;

procedure BTLinkKeyRequestReply (BT : PBTDevice; Addr : TBDAddr; Key : TBDKey);
var
  Params : array of byte;
  i : integer;
begin
  SetLength (Params, 22);
  Params[0] := Addr[5];
  Params[1] := Addr[4];
  Params[2] := Addr[3];
  Params[3] := Addr[2];
  Params[4] := Addr[1];
  Params[5] := Addr[0];
  for i := 0 to 15 do Params[21 - i] := Key[i];
  BTAddCommand (BT, OGF_LINK_CONTROL, $0b, Params);
end;

procedure BTLinkKeyRequestNegativeReply (BT : PBTDevice; Addr : TBDAddr);
var
  Params : array of byte;
begin
  SetLength (Params, 6);
  Params[0] := Addr[5];
  Params[1] := Addr[4];
  Params[2] := Addr[3];
  Params[3] := Addr[2];
  Params[4] := Addr[1];
  Params[5] := Addr[0];
  BTAddCommand (BT, OGF_LINK_CONTROL, $0c, Params);
end;

procedure BTLinkCodeRequestReply (BT : PBTDevice; Addr : TBDAddr; Code : string);
var
  Params : array of byte;
  i : integer;
begin
  SetLength (Params, 7 + length (Code));
  Params[0] := Addr[5];
  Params[1] := Addr[4];
  Params[2] := Addr[3];
  Params[3] := Addr[2];
  Params[4] := Addr[1];
  Params[5] := Addr[0];
  Params[6] := length (Code);
  for i := 1 to length (Code) do Params[6 + i] := ord (Code[i]);
  BTAddCommand (BT, OGF_LINK_CONTROL, $0d, Params);
end;


procedure BTAuthenticationRequested (BT : PBTDevice; Handle : Word);
var
  Params : array of byte;
begin
  SetLength (Params, 2);
  Params[0] := lo (Handle);
  Params[1] := hi (Handle);
  BTAddCommand (BT, OGF_LINK_CONTROL, $11, Params);
end;


// Controller / Baseband

procedure BTWriteSimplePairingMode (BT : PBTDevice; Mode : byte);
begin
  BTAddCommand (BT, OGF_HOST_CONTROL, $56, [Mode]);
end;

procedure BTReadLocalOOBData (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_HOST_CONTROL, $57, []);
end;

// informational parameters
procedure BTReadLocalVersion (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_INFORMATIONAL, $01, []);
end;

procedure BTReadLocalSupportedCommands (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_INFORMATIONAL, $02, []);
end;

procedure BTReadLocalSupportedFeatures (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_INFORMATIONAL, $03, []);
end;

procedure BTReadBDADDR (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_INFORMATIONAL, $09, []);
end;

// le control
procedure BTSetLEEventMask (BT : PBTDevice; Mask : QWord);
var
  Params : array of byte;
  MaskHi, MaskLo : DWord;
begin
  MaskHi := hi (Mask);
  MaskLo := lo (Mask);
  SetLength (Params, 8);
  Params[0] := MaskLo and $ff;   // lsb
  Params[1] := (MaskLo shr 8) and $ff;
  Params[2] := (MaskLo shr 16) and $ff;
  Params[3] := (MaskLo shr 24) and $ff;
  Params[4] := MaskHi and $ff;   // lsb
  Params[5] := (MaskHi shr 8) and $ff;
  Params[6] := (MaskHi shr 16) and $ff;
  Params[7] := (MaskHi shr 24) and $ff;
  BTAddCommand (BT, OGF_LE_CONTROL, $01, Params);
end;

procedure BTReadLEBufferSize (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_LE_CONTROL, $02, []);
end;

procedure BTReadLESupportedFeatures (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_LE_CONTROL, $03, []);
end;

procedure BTSetLERandomAddress (BT : PBTDevice; Addr : TBDAddr);
begin
  BTAddCommand (BT, OGF_LE_CONTROL, $05, [Addr[5], Addr[4], Addr[3], Addr[2], Addr[1], Addr[0]]);
end;

procedure BTSetLEAdvertisingParameters (BT : PBTDevice; MinInterval, MaxInterval : Word;
                                     Type_ : byte;
                                     OwnAddressType, PeerAddressType : byte;
                                     PeerAddr : TBDAddr;
                                     ChannelMap, FilterPolicy : byte);
begin
  BTAddCommand (BT, OGF_LE_CONTROL, $06, [lo (MinInterval), hi (MinInterval),
                                       lo (MaxInterval), hi (MaxInterval),
                                       Type_, OwnAddressType, PeerAddressType,
                                       PeerAddr[0], PeerAddr[1], PeerAddr[2],
                                       PeerAddr[3], PeerAddr[4], PeerAddr[5],
                                       ChannelMap, FilterPolicy]);
end;

procedure BTReadLEAdvertisingChannelTxPower (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_LE_CONTROL, $07, []);
end;

procedure BTSetLEAdvertisingData (BT : PBTDevice; Data : array of byte);
var
  Params : array of byte;
  Len : byte;
  i : integer;
begin
   SetLength (Params, 32);
   for i := 1 to 31 do Params[i] := 0;       // clear data
   Len := length (Data);
   if Len > 31 then Len := 31;
   Params[0] := Len;
   for i := 0 to Len - 1 do Params[i + 1] := Data[i];
   BTAddCommand (BT, OGF_LE_CONTROL, $08, Params);
end;

procedure BTSetLEScanResponseData (BT : PBTDevice; Data : array of byte);
var
  Params : array of byte;
  Len : byte;
  i : integer;
begin
   SetLength (Params, 32);
   for i := 1 to 31 do Params[i] := 0;       // clear data
   Len := length (Data);
   if Len > 31 then Len := 31;
   Params[0] := Len;
   for i := 0 to Len - 1 do Params[i + 1] := Data[i];
   BTAddCommand (BT, OGF_LE_CONTROL, $09, Params);
end;

procedure BTSetLEAdvertisingEnable (BT : PBTDevice; State : boolean);
begin
  if State then
    BTAddCommand (BT, OGF_LE_CONTROL, $0a, [$01])
  else
    BTAddCommand (BT, OGF_LE_CONTROL, $0a, [$00]);
end;

procedure BTSetLEScanParameters (BT : PBTDevice; Type_ : byte; Interval, Window : Word;
                                 OwnAddressType, FilterPolicy : byte);
begin
  BTAddCommand (BT, OGF_LE_CONTROL, $0b, [Type_, lo (Interval), hi (Interval),
                lo (Window), hi (Window), OwnAddressType, FilterPolicy]);
end;

procedure BTSetLEScanEnable (BT : PBTDevice; State, Duplicates : boolean);
var
  Params : array of byte;
begin
  SetLength (Params, 2);
  if State then Params[0] := $01 else Params[0] := $00;
  if Duplicates then Params[1] := $01 else Params[1] := $00;
  BTAddCommand (BT, OGF_LE_CONTROL, $0c, Params);
end;

procedure BTLERand (BT : PBTDevice);
begin
  BTAddCommand (BT, OGF_LE_CONTROL, $18, []);
end;

procedure BTStartLEEncryption (BT : PBTDevice; Handle : Word; Rand : QWord; Diversifier : Word; Key : TLEKey);
begin
  // todo
end;

function ErrToStr (code : byte) : string;
begin               // page 377 onwards 4.2
  case code of
    $00 : Result := 'Success';
    $01 : Result := 'Unknown HCI Command';
    $02 : Result := 'Unknown Connection Identifier';
    $03 : Result := 'Hardware Failure';
    $04 : Result := 'Page Timeout';
    $05 : Result := 'Authentication Failure';
    $06 : Result := 'PIN or Key Missing';
    $07 : Result := 'Memory Capacity Exceeded';
    $08 : Result := 'Connection Timeout';
    $09 : Result := 'Connection Limit Exceeded';
    $0A : Result := 'Synchronous Connection Limit To A Device Exceeded';
    $0B : Result := 'ACL Connection Already Exists';
    $0C : Result := 'Command Disallowed';
    $0D : Result := 'Connection Rejected due to Limited ';
    $0E : Result := 'Connection Rejected due To Security Reasons';
    $0F : Result := 'Connection Rejected due to Unacceptable BD_ADDR';
    $10 : Result := 'Connection Accept Timeout Exceeded';
    $11 : Result := 'Unsupported Feature or Parameter Value';
    $12 : Result := 'Invalid HCI Command Parameters';
    $13 : Result := 'Remote User Terminated Connection';
    $14 : Result := 'Remote Device Terminated Connection due to Low Resources';
    $15 : Result := 'Remote Device Terminated Connection due to Power Off';
    $16 : Result := 'Connection Terminated By Local Host';
    $17 : Result := 'Repeated Attempts';
    $18 : Result := 'Pairing Not Allowed';
    $19 : Result := 'Unknown LMP PDU';
    $1A : Result := 'Unsupported Remote Feature / Unsupported LMP Feature';
    $1B : Result := 'SCO Offset Rejected';
    $1C : Result := 'SCO Interval Rejected';
    $1D : Result := 'SCO Air Mode Rejected';
    $1E : Result := 'Invalid LMP Parameters / Invalid LL Parameters';
    $1F : Result := 'Unspecified Error';
    $20 : Result := 'Unsupported LMP Parameter Value / Unsupported LL Parameter Value';
    $21 : Result := 'Role Change Not Allowed';
    $22 : Result := 'LMP Response Timeout / LL Response Timeout';
    $23 : Result := 'LMP Error Transaction Collision';
    $24 : Result := 'LMP PDU Not Allowed';
    $25 : Result := 'Encryption Mode Not Acceptable';
    $26 : Result := 'Link Key cannot be Changed';
    $27 : Result := 'Requested QoS Not Supported';
    $28 : Result := 'Instant Passed';
    $29 : Result := 'Pairing With Unit Key Not Supported';
    $2A : Result := 'Different Transaction Collision';
    $2B : Result := 'Reserved';
    $2C : Result := 'QoS Unacceptable Parameter';
    $2D : Result := 'QoS Rejected';
    $2E : Result := 'Channel Classification Not Supported';
    $2F : Result := 'Insufficient Security';
    $30 : Result := 'Parameter Out Of Mandatory Range';
    $31 : Result := 'Reserved';
    $32 : Result := 'Role Switch Pending';
    $33 : Result := 'Reserved';
    $34 : Result := 'Reserved Slot Violation';
    $35 : Result := 'Role Switch Failed';
    $36 : Result := 'Extended Inquiry Response Too Large';
    $37 : Result := 'Secure Simple Pairing Not Supported By Host';
    $38 : Result := 'Host Busy - Pairing';
    $39 : Result := 'Connection Rejected due to No Suitable Channel Found';
    $3A : Result := 'Controller Busy';
    $3B : Result := 'Unacceptable Connection Parameters';
    $3C : Result := 'Directed Advertising Timeout';
    $3D : Result := 'Connection Terminated due to MIC Failure';
    $3E : Result := 'Connection Failed to be Established';
    $3F : Result := 'MAC Connection Failed';
    $40 : Result := 'Coarse Clock Adjustment Rejected but Will Try to Adjust Using Clock';
    else  Result := 'Unknown ' + IntToHex (code, 2);
    end;
end;

initialization
  SetLength (RxBuffer, 0);

end.

