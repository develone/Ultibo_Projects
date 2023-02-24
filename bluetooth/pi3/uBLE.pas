unit uBLE;

{$mode delphi}{$H+}
// { $define show_data}

interface

uses
  Classes, SysUtils, uBT;

const
  STANDBY_STATE               = 1;
  ADVERTISING_STATE           = 2;
  SCANNING_STATE              = 3;
  INITIATING_STATE            = 4;
  CONNECTION_STATE            = 5;

  // Markers
  INITIAL_SETUP_DONE          = 1;

  // HCI Funcional Specification Section 7.8.10, Core 4.1 page 1255
  LL_SCAN_PASSIVE		          =	$00;
  LL_SCAN_ACTIVE		          = $01;

  //  BLUETOOTH SPECIFICATION Version 4.2 [Vol 2, Part E] page 970
  // advertising type
  ADV_IND                     = $00; // Connectable undirected advertising (default)
  ADV_DIRECT_IND_HI           = $01; // Connectable high duty cycle directed advertising
  ADV_SCAN_IND                = $02; // Scannable undirected advertising
  ADV_NONCONN_IND             = $03; // Non connectable undirected advertising
  ADV_DIRECT_IND_LO           = $04; // Connectable low duty cycle directed advertising
  // $05 – $FF Reserved for future use

  // own address type
  LL_ADR_PUBLIC               = $00; // Public Device Address (default)
  ll_ADR_RANDOM               = $01; // Random Device Address
  LL_ADR_PRIVATE_PUBLIC       = $02; // Controller generates Resolvable Private Address based on the local
                                     // IRK from resolving list. If resolving list contains no matching entry,
                                     // use public address.
  LL_ADR_PRIVATE_RANDOM       = $03; // Controller generates Resolvable Private Address based on the local
                                     // IRK from resolving list. If resolving list contains no matching entry,
                                     // use random address from LE_Set_Random_Address.
  // $04 – $FF Reserved for future use
  // peer address type
  LL_PEER_PUBLI               = $00; // Public Device Address (default) or Public Identity Address
  LL_PEER_RANDOM              = $01; // Random Device Address or Random (static) Identity Address
  // $02 – $FF Reserved for future use
  (*
  Value Parameter Description
  0xXXXXXXXXXXXX Public Device Address, Random Device Address, Public Identity
  Address, or Random (static) Identity Address of the device to be
  connected        *)

  LL_CHAN37                   = $01;
  LL_CHAN38                   = $02;
  LL_CHAN39                   = $04;
  LL_ALL_CHANS                = LL_CHAN37 or LL_CHAN38 or LL_CHAN39;

  // Advertising Data Types
  ADT_FLAGS                   = $01;      // Flags
  ADT_INCOMPLETE_UUID16       = $02;      // Incomplete List of 16-bit Service Class UUIDs
  ADT_COMPLETE_UUID16         = $03;      // Complete List of 16-bit Service Class UUIDs
  ADT_INCOMPLETE_UUID32       = $04;      // Incomplete List of 32-bit Service Class UUIDs
  ADT_COMPLETE_UUID32         = $05;      // Complete List of 32-bit Service Class UUIDs
  ADT_INCOMPLETE_UUID128      = $06;      // Incomplete List of 128-bit Service Class UUIDs
  ADT_COMPLETE_UUDI128        = $07;      // Complete List of 128-bit Service Class UUIDs
  ADT_SHORTENED_LOCAL_NAME    = $08;      // Shortened Local name
  ADT_COMPLETE_LOCAL_NAME     = $09;      // Complete Local name


  ADT_POWER_LEVEL             = $0A;      // Tx Power Level
  ADT_DEVICE_CLASS            = $0D;      // Class of Device


  ADT_MANUFACTURER_SPECIFIC   = $FF;
                (*
0x01 	«Flags» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.3 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.3 and 18.1 (v4.0)Core Specification Supplement, Part A, section 1.3
0x02 	«Incomplete List of 16-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x03 	«Complete List of 16-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x04 	«Incomplete List of 32-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, section 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x05 	«Complete List of 32-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, section 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x06 	«Incomplete List of 128-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x07 	«Complete List of 128-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x08 	«Shortened Local Name» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.2 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.2 and 18.4 (v4.0)Core Specification Supplement, Part A, section 1.2
0x09 	«Complete Local Name» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.2 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.2 and 18.4 (v4.0)Core Specification Supplement, Part A, section 1.2
0x0A 	«Tx Power Level» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.5 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.3 (v4.0)Core Specification Supplement, Part A, section 1.5
0x0D 	«Class of Device» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)Core Specification Supplement, Part A, section 1.6
0x0E 	«Simple Pairing Hash C» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)
0x0E 	«Simple Pairing Hash C-192» 	Core Specification Supplement, Part A, section 1.6
0x0F 	«Simple Pairing Randomizer R» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)
0x0F 	«Simple Pairing Randomizer R-192» 	Core Specification Supplement, Part A, section 1.6
0x10 	«Device ID» 	Device ID Profile v1.3 or later
0x10 	«Security Manager TK Value» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.7 and 18.6 (v4.0)Core Specification Supplement, Part A, section 1.8
0x11 	«Security Manager Out of Band Flags» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.6 and 18.7 (v4.0)Core Specification Supplement, Part A, section 1.7
0x12 	«Slave Connection Interval Range» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.8 and 18.8 (v4.0)Core Specification Supplement, Part A, section 1.9
0x14 	«List of 16-bit Service Solicitation UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.9 and 18.9 (v4.0)Core Specification Supplement, Part A, section 1.10
0x1F 	«List of 32-bit Service Solicitation UUIDs» 	Core Specification Supplement, Part A, section 1.10
0x15 	«List of 128-bit Service Solicitation UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.9 and 18.9 (v4.0)Core Specification Supplement, Part A, section 1.10
0x16 	«Service Data» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.10 and 18.10 (v4.0)
0x16 	«Service Data - 16-bit UUID» 	Core Specification Supplement, Part A, section 1.11
0x20 	«Service Data - 32-bit UUID» 	Core Specification Supplement, Part A, section 1.11
0x21 	«Service Data - 128-bit UUID» 	Core Specification Supplement, Part A, section 1.11
0x22 	«LE Secure Connections Confirmation Value» 	Core Specification Supplement Part A, Section 1.6
0x23 	«LE Secure Connections Random Value» 	Core Specification Supplement Part A, Section 1.6
0x24 	«URI» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.18
0x25 	«Indoor Positioning» 	Indoor Posiioning Service v1.0 or later
0x26 	«Transport Discovery Data» 	Transport Discovery Service v1.0 or later
0x17 	«Public Target Address» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.13
0x18 	«Random Target Address» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.14
0x19 	«Appearance» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.12
0x1A 	«Advertising Interval» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.15
0x1B 	«LE Bluetooth Device Address» 	Core Specification Supplement, Part A, section 1.16
0x1C 	«LE Role» 	Core Specification Supplement, Part A, section 1.17
0x1D 	«Simple Pairing Hash C-256» 	Core Specification Supplement, Part A, section 1.6
0x1E 	«Simple Pairing Randomizer R-256» 	Core Specification Supplement, Part A, section 1.6
0x3D 	«3D Information Data» 	3D Synchronization Profile, v1.0 or later
0xFF 	«Manufacturer Specific Data» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.4 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.4 and 18.11 (v4.0)Core Specification Supplement, Part A, section 1.4
0x27 	«LE Supported Features» 	Core Specification Supplement, Part A, Section 1.19
0x28 	«Channel Map Update Indication» 	Core Specification Supplement, Part A, Section 1.20


0x01	Flags	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.3 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.3 and 18.1 (v4.0)Core Specification Supplement, Part A, section 1.3
0x02	Incomplete List of 16-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x03	Complete List of 16-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x04	Incomplete List of 32-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, section 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x05	Complete List of 32-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, section 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x06	Incomplete List of 128-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x07	Complete List of 128-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x08	Shortened Local Name	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.2 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.2 and 18.4 (v4.0)Core Specification Supplement, Part A, section 1.2
0x09	Complete Local Name	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.2 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.2 and 18.4 (v4.0)Core Specification Supplement, Part A, section 1.2
0x0A	Tx Power Level	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.5 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.3 (v4.0)Core Specification Supplement, Part A, section 1.5
0x0D	Class of Device	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)Core Specification Supplement, Part A, section 1.6
0x0E	Simple Pairing Hash C	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)
0x0E	Simple Pairing Hash C-192	Core Specification Supplement, Part A, section 1.6
0x0F	Simple Pairing Randomizer R	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)
0x0F	Simple Pairing Randomizer R-192	Core Specification Supplement, Part A, section 1.6
0x10	Device ID	Device ID Profile v1.3 or later
0x10	Security Manager TK Value	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.7 and 18.6 (v4.0)Core Specification Supplement, Part A, section 1.8
0x11	Security Manager Out of Band Flags	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.6 and 18.7 (v4.0)Core Specification Supplement, Part A, section 1.7
0x12	Slave Connection Interval Range	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.8 and 18.8 (v4.0)Core Specification Supplement, Part A, section 1.9
0x14	List of 16-bit Service Solicitation UUIDs	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.9 and 18.9 (v4.0)Core Specification Supplement, Part A, section 1.10
0x15	List of 128-bit Service Solicitation UUIDs	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.9 and 18.9 (v4.0)Core Specification Supplement, Part A, section 1.10
0x16	Service Data	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.10 and 18.10 (v4.0)
0x16	Service Data - 16-bit UUID	Core Specification Supplement, Part A, section 1.11
0x17	Public Target Address	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.13
0x18	Random Target Address	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.14
0x19	Appearance	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.12
0x1A	Advertising Interval	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.15
0x1B	LE Bluetooth Device Address	Core Specification Supplement, Part A, section 1.16
0x1C	LE Role	Core Specification Supplement, Part A, section 1.17
0x1D	Simple Pairing Hash C-256	Core Specification Supplement, Part A, section 1.6
0x1E	Simple Pairing Randomizer R-256	Core Specification Supplement, Part A, section 1.6
0x1F	List of 32-bit Service Solicitation UUIDs	Core Specification Supplement, Part A, section 1.10
0x20	Service Data - 32-bit UUID	Core Specification Supplement, Part A, section 1.11
0x21	Service Data - 128-bit UUID	Core Specification Supplement, Part A, section 1.11
0x22	LE Secure Connections Confirmation Value	Core Specification Supplement Part A, Section 1.6
0x23	LE Secure Connections Random Value	Core Specification Supplement Part A, Section 1.6
0x24	URI	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.18
0x25	Indoor Positioning	Indoor Posiioning Service v1.0 or later
0x26	Transport Discovery Data	Transport Discovery Service v1.0 or later
0x3D	3D Information Data	3D Synchronization Profile, v1.0 or later
0xFF	Manufacturer Specific Data	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.4 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.4 and 18.11 (v4.0)Core Specification Supplement, Part A, section 1.4


*)

// ADVERTISING_CHANNEL_ADDRESS = $8E89BED6;   // 4.2 pg 38

  // Company IDs
  ID_APPLE                    = $004C;

  SCAN_WINDOW 		            =	200000;
  SCAN_INTERVAL			          = 500000;

//const
//  ADVERTISING_CHANNEL_ADDRESS : TADV_ADDRESS = ($00, $00, $8E, $89, $BE, $D6);   // 4.2 pg 38

type
  TBTBeaconEvent = procedure (UUID : string; Major, Minor : word; Rssi : byte);


var
  LLState : integer = STANDBY_STATE; // link layer state


procedure BLEInitialSetup (BT : PBTDevice);               // page 133 v4.2
procedure BLEStartUndirectedAdvertising (BT : PBTDevice); // page 136 v4.2
procedure BLEStartDirectedAdvertising (BT : PBTDevice);   // page 137 v4,2
procedure BLEStartPassiveScanning (BT : PBTDevice);       // page 139 v4.2
procedure BLEStartActiveScanning (BT : PBTDevice);        // page 140 v4.2
procedure BLEStopScanning (BT : PBTDevice);
procedure BLEStopAdvertising (BT : PBTDevice);

function BLEAdvertisingTypeToStr (Type_ : byte) : string;

// helper functions
procedure BLESetBeaconEvent (anEvent : TBTBeaconEvent);
procedure BLEClearAdvertisingData (BT : PBTDevice);
procedure BLEAddAdvertisingData (BT : PBTDevice; Type_ : byte); overload;
procedure BLEAddAdvertisingData (BT : PBTDevice; Type_ : byte; Data : array of byte); overload;
procedure BLEAddAdvertisingData (BT : PBTDevice; Type_ : byte; Data : string); overload;

implementation

uses uLog;

var
  BeaconEvent : TBTBeaconEvent = nil;

procedure BLESetBeaconEvent (anEvent : TBTBeaconEvent);
begin
  BeaconEvent := anEvent;
end;

procedure BLEInitialSetup (BT : PBTDevice);
begin
  BTNoOP (BT);
  BTReadLocalSupportedCommands (BT);
  BTReadLocalSupportedFeatures (BT);
  BTSetLEEventMask (BT, $ff);
  BTReadLEBufferSize (BT);
  BTReadLESupportedFeatures (BT);
  BTReadBDADDR (BT);
//  AddMarker (INITIAL_SETUP_DONE);
end;

function BLEAdvertisingTypeToStr (Type_ : byte) : string;
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

procedure BLEStartUndirectedAdvertising (BT : PBTDevice);
begin
  BTReadLEAdvertisingChannelTxPower (BT);
  BTSetLEAdvertisingData (BT, BT.AdData);
  BTSetLEAdvertisingEnable (BT, true);
 // AddMarker (ADVERTISING_STATE);
end;

procedure BLEStartDirectedAdvertising (BT : PBTDevice);
begin
  //
end;

procedure BLEStopAdvertising (BT : PBTDevice);
begin
  BTSetLEAdvertisingEnable (BT, false);
end;

procedure BLEStartPassiveScanning (BT : PBTDevice);
begin
//  BTSetLEScanParameters (BT, LL_SCAN_PASSIVE, 200, 100, $00, $00);
  //BTSetLEScanParameters (BT, LL_SCAN_PASSIVE, 2000, 1000, $00, $00);
  BTSetLEScanParameters (BT, LL_SCAN_PASSIVE, 10, 10, $00, $00);
  BTSetLEScanEnable (BT, true, true);
end;

procedure BLEStartActiveScanning (BT : PBTDevice);
begin
 // BTSetLEScanParameters (BT, LL_SCAN_ACTIVE, SCAN_INTERVAL div 1000, SCAN_WINDOW div 1000, $00, $00);
  BTSetLEScanParameters (BT, LL_SCAN_ACTIVE, 10, 10, $00, $00);
  BTSetLEScanEnable (BT, true, true);
end;

procedure BLEStopScanning (BT : PBTDevice);
begin
  BTSetLEScanEnable (BT, false, false);
end;

procedure BLEClearAdvertisingData (BT : PBTDevice);
begin
  SetLength (BT.AdData, 0);
end;

procedure BLEAddAdvertisingData (BT : PBTDevice; Type_ : byte);
begin
  BLEAddAdvertisingData (BT, Type_, []);
end;

procedure BLEAddAdvertisingData (BT : PBTDevice; Type_ : byte; Data : array of byte);
var
  Len : byte;
  i : integer;
begin
  Len := Length (BT.AdData);
  SetLength (BT.AdData, Len + length (Data) + 2);
  BT.AdData[Len] := Length (Data) + 1;
  BT.AdData[Len + 1] := Type_;
  for i := 0 to high (Data) do BT.AdData[Len + 2 + i] := Data[i];
end;

procedure BLEAddAdvertisingData (BT : PBTDevice; Type_ : byte; Data : string);
var
  Len : byte;
  i : integer;
begin
  Len := Length (BT.AdData);
  SetLength (BT.AdData, Len + length (Data) + 2);
  BT.AdData[Len] := Length (Data) + 1;
  BT.AdData[Len + 1] := Type_;
  for i := 1 to length (Data) do BT.AdData[Len + 1 + i] := ord (Data[i]);
end;

function UUIDToStr (uuid : array of byte) : string;
begin
  if length (uuid) = 16 then
    Result := format ('%.2X%.2X%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X',
                      [uuid[0], uuid[1], uuid[2], uuid[3], uuid[4], uuid[5], uuid[6], uuid[7],
                       uuid[8], uuid[9], uuid[10], uuid[11], uuid[12], uuid[13], uuid[14], uuid[15]])
  else
    Result := '';
end;

procedure DecodeADS (ads : array of byte);
var
  uuid : array of byte;
  len : integer;
begin
{$ifdef show_data}
  s := '';
  for i := low (ads) to high (ads) do
    s := s + ' ' + IntToHex (ads[i], 2);
  log ('ADS len ' + length (ads).ToString + ' - ' + s);
{$endif}
  len := length (ads);
  if len = 0 then exit;
  case ads[0] of
    ADT_FLAGS                 : ;
    ADT_SHORTENED_LOCAL_NAME  : ;
    ADT_COMPLETE_LOCAL_NAME   : ;
    ADT_MANUFACTURER_SPECIFIC :
      begin
        if len = 26 then
          begin            // id = apple                  type = 2       length = 21
            if (ads[1] = $4c) and (ads[2] = $00) and (ads[3] = $02) and (ads[4] = $15) then
              begin // looks awfully like a beacon
                if Assigned (BeaconEvent) then
                  begin
                    SetLength (uuid, 16);
                    Move (ads[5], uuid[0], 16);
                    BeaconEvent (UUIDToStr (uuid), ads[21] * $100 + ads[22], ads[23] * $100 + ads[24], ads[25]);
                  end;
              end;
          end;
      end;
  end;
end;

procedure DecodeReport (Report : array of byte);
var
{$ifdef show_data}
  s : string;
{$endif}
  i, len : integer;
  ads : array of byte;
  gl : boolean;        // getting length
begin
{$ifdef show_data}
  s := '';
  for i := low (Report) to high (Report) do
    s := s + ' ' + IntToHex (Report[i], 2);
  log ('Report ' + s);
{$endif}
  gl := true;
  len := 0;
  i := low (Report);
  while i <= high (Report) do
    begin
      if gl then
        begin
          gl := false;
          len := Report[i];
          i := i + 1;
          if len = 0 then break;
        end
      else if (len + i - 1 <= high (Report)) then
        begin
          SetLength (ads, len);
          Move (Report[i], ads[0], len);
          i := i + len;
          DecodeADS (ads);
          gl := true;
        end
      else
        begin
          Log ('Error decoding AD structure');
          break;
        end;
    end; // while
end;

procedure DoLEEvent (SubEvent : byte; Params : array of byte);
var
  i, ofs, len, rl : integer;
  nr : byte;
  rpt : array of byte;
{$ifdef show_data}
  s : string;
{$endif}
begin
  len := length (Params);
{$ifdef show_data}
  s := '';
  for i := low (Params) to high (Params) do
    s := s + ' ' + Params[i].ToHexString (2);
  Log ('LEEvent ' + SubEvent.ToHexString (2) + ' Params ' + s);
{$endif}
  case SubEvent of
    $02 : // le advertsing report
      begin
        nr := Params[0]; // num reports
        ofs := 1;
//      log ('  ' + nr.ToString + ' advertising reports');
        for i := 1 to nr do
          begin
            if ofs + 8 <= len then
              begin
                rl := Params[ofs + 8]; // length
                if ofs + 9 + rl <= len then
                  begin
                (*    Log ('Report ' + i.ToString + ' Address ' +
                          Params[ofs + 7].ToHexString (2) + ':' +
                          Params[ofs + 6].ToHexString (2) + ':' +
                          Params[ofs + 5].ToHexString (2) + ':' +
                          Params[ofs + 4].ToHexString (2) + ':' +
                          Params[ofs + 3].ToHexString (2) + ':' +
                          Params[ofs + 2].ToHexString (2));   *)
                    SetLength (rpt, rl);
                    Move (Params[ofs + 9], rpt[0], rl);
                    DecodeReport (rpt);
                    ofs := ofs + rl + 9;
                  end
                else
                  begin
                    Log ('Invalid Report');
                    break;
                  end;
              end;
//          Log ('Event Type ' + EventTypeToStr (ev[ofs + 1]));
//          Log ('Address Type ' + ev[ofs + 2].ToHexString);
          end;
        end;
  end;
end;

(*   // ibeacon         http://www.argenox.com/a-ble-advertising-primer/
Setup the add packet flags:

  1E
  02 		  # Number of bytes that follow in first AD structure
  01  	  # Flags AD type
  1A  	  # Flags value 0x1A = 000011010
              bit 0 (OFF) LE Limited Discoverable Mode
              bit 1 (ON) LE General Discoverable Mode
              bit 2 (OFF) BR/EDR Not Supported
              bit 3 (ON) Simultaneous LE and BR/EDR to Same Device Capable (controller)
              bit 4 (ON) Simultaneous LE and BR/EDR to Same Device Capable (Host)

  1A  	  # Number of bytes that follow in second (and last) AD structure
Define vendor specific values:

  FF 		  # Manufacturer specific data AD type
  4C 00   # Company identifier code (0x004C == Apple)
  02 		  # Byte 0 of iBeacon advertisement indicator
  15 		  # Byte 1 of iBeacon advertisement indicator
Our specific UUID values:

  63 6F 3F 8F 64 91 4B EE 95 F7 D8 CC 64 A8 63 B5 # our iBeacon proximity uuid
  00 00 	# Major
  00 00 	# Minor
  C8 00 	# Calibrated Tx power
*)


initialization

end.

