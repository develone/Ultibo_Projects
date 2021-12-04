program DWT_LIFT_RPi;

{$mode objfpc}{$H+}
{define RPI}
uses
 RaspberryPi, {<-- Change this to suit which model you have!!}
 GlobalConfig,
 GlobalConst,
 GlobalTypes,
 Platform,
 Threads,
 Console,
 SysUtils,  { TimeToStr & Time }
 { needed by bitmap }
 GraphicsConsole, {Include the GraphicsConsole unit so we can create a graphics window}
 BMPcomn,         {Include the BMPcomn unit from the fpc-image package to give the Bitmap headers}
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
 uLiftBitmap,
 Logging,
 BCM2835,
 BCM2708,
 network,
 overrides,
 wifidevice,
 StrUtils,
 gpio,
 devices,
 ultibo,
 Syscalls;

{$linklib dwtlift}
{$linklib libm}

procedure decom_test(x0,y0,x1,y1:LongWord;fn:string); cdecl; external 'libdwtlift' name 'decom_test';
var
  {Variables needed for WIFI}
SSID : string;
key : string;
Country : string;
ScanResultList : TStringList;
Winsock2TCPClient : TWinsock2TCPClient;
CYW43455Network: PCYW43455Network;
BSSIDStr : string;
i : integer;
BSSID : ether_addr;
Status : Longword;
{Variables needed for WIFI
WIFIScanCallback }

 Handle:THandle;
 Handle1:THandle;
 {Handle2:THandle;}
 Window:TWindowHandle;


 IPAddress : string;

 DECOMP: Integer;
 ENCODE: Integer;
 TCP_DISTORATIO: Integer;
 FILTER: Integer;
 COMPRESSION_RATIO : Integer;
 DIS_CR_FLG : Integer;
 X:LongWord;
 Y:LongWord;
 Width:LongWord;
 Height:LongWord;
 da_x0,da_y0,da_x1,da_y1:LongWord;
 ff:string;
 {
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
}
procedure WIFIScanCallback(ssid : string; ScanResultP : pwl_escan_result);
var
  ssidstr : string;
begin
  ssidstr := ssid + ' ' + inttohex(ScanResultP^.bss_info[1].BSSID.octet[0],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[1],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[2],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[3],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[4],2) + ':'
                            + inttohex(ScanResultP^.bss_info[1].BSSID.octet[5],2);

  if (ScanResultList <> nil) and (ScanResultList.Indexof(ssidstr) < 0) then
    ScanResultList.Add(ssidstr);
end;

procedure WaitForIP;
begin
  Winsock2TCPClient:=TWinsock2TCPClient.Create;

  while (true) do
  begin
    sleep(200);
    if (Winsock2TCPClient.LocalAddress <> IPAddress)
       and (length(Winsock2TCPClient.LocalAddress) > 0)
       and (Winsock2TCPClient.LocalAddress <> ' ') then
    begin
      ConsoleWindowWriteLn(Handle, 'IP address='+Winsock2TCPClient.LocalAddress);
      IPAddress := Winsock2TCPClient.LocalAddress;
      break;
    end;
  end;
end;

procedure Msg (Sender : TObject; s : string);

begin

  ConsoleWindowWriteLn (Handle1, s);

end;



procedure WaitForSDDrive;

begin

  while not DirectoryExists ('C:\') do sleep (500);

end;



begin

{
 The following 3 lines are logging to the console
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
 }

 {The following 2 lines are logging to a file}
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));


 // wait for IP address and SD Card to be initialised.
 WaitForSDDrive;
 {IPAddress := WaitForIPComplete;}
 {Wait a few seconds for all initialization (like filesystem and network) to be done}
 Sleep(5000);

 {Create a graphics window to display our bitmap, let's use the new CONSOLE_POSITION_FULLSCREEN option}
 Window:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOM);

 {Call our bitmap drawing function and pass the name of our bitmap file on the SD card,
  we also pass the handle for our graphics console window and the X and Y locations to
  draw the bitmap.

  What happens if the bitmap is bigger than the window? It will be trimmed to fit, try it
  yourself and see}

 Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);
 Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);
 ConsoleWindowWriteLn (Handle1, 'TFTP Demo.');
 {Handle2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);}
 //Handle3:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMRIGHT,True);
 ConsoleWindowWriteLn(Handle1, 'writing top right handle1');
 {ConsoleWindowWriteLn(Handle2, 'writing bottom left handle2');}
 //ConsoleWindowWriteLn(Handle3, 'writing bottom right handle3');
 ConsoleWindowWriteLn(Handle, TimeToStr(Time));
  WIFIPreInit;

  // We've gotta wait for the file system to be alive because that's where the firmware is.
  // Because the WIFI uses the Arasan host, the only way you'll get a drive C
  // is if you use USB boot. So that's a pre-requisite at the moment until we make the
  // SD card work off the other SDHost controller.

  ConsoleWindowWriteln(Handle, 'Waiting for file system...');
  Sleep(5000);
  while not directoryexists('c:\') do
  begin
    Sleep(0);
  end;
  ConsoleWindowWriteln(Handle, 'File system ready. Initialize Wifi Device.');

  try
    // WIFIInit has to be done from the main application because the initialisation
    // process needs access to the c: drive in order to load the firmware, regulatory file
    // and configuration file.
    // There is the option of adding the files as binary blobs to be compiled into
    // the kernel, but that would need to be an option I think really (easily done
    // by choosing to add a specific unit to the uses clause)
    // We'll need to work out what the best solution is later.

    WIFIInit;

    // warning, after wifiinit is called, the deviceopen() stuff will happen on
    // a different thread, so the code below will execute regardless of whether
    // the device is open or not. Consequently we are going to spin until the
    // wifi device has been fully initialized. This is a bit of a dirty hack
    // but hopefully we can change it to a proper 'link is up' check once the
    // whole network device integration stuff is complete.
    // Certainly can't stay the way it is.

    ConsoleWindowWriteln(Handle, 'Waiting for Wifi Device to be opened.');

    // spin until the wifi device is actually ready to do stuff.
    repeat
      CYW43455Network := PCYW43455Network(NetworkDeviceFindByDescription(CYW43455_NETWORK_DESCRIPTION));
      if CYW43455Network = nil then
        Sleep(100);
    until CYW43455Network <> nil;

    while CYW43455Network^.Network.NetworkState <> NETWORK_STATE_OPEN do
    begin
      Sleep(0);
    end;


    if (SysUtils.GetEnvironmentVariable('WIFISCAN') = '1') then
    begin
      ConsoleWindowWriteln(Handle, 'Performing a WIFI network scan...');
      ScanResultList := TStringList.Create;

      WirelessScan(@WIFIScanCallback);

      for i := 0 to ScanResultList.Count-1 do
        ConsoleWindowWriteln(Handle, 'Found access point: ' + ScanResultList[i]);

      ScanResultList.Free;
    end
    else
      ConsoleWindowWriteln(Handle, 'Network scan not enabled in cmdline.txt (add the WIFISCAN=1 entry)');

    SSID := SysUtils.GetEnvironmentVariable('SSID');
    key := SysUtils.GetEnvironmentVariable('KEY');
    Country := SysUtils.GetEnvironmentVariable('COUNTRY');
    BSSIDStr := SysUtils.GetEnvironmentVariable('BSSID');

    ConsoleWindowWriteln(Handle, 'Attempting to join WIFI network ' + SSID + ' (Country='+Country+')');

    if (Key = '') then
      ConsoleWindowWriteln(Handle, 'Warning: Key not specified - expecting the network to be unencrypted.');

    if (SSID = '') or (Country='') then
       ConsoleWindowWriteln(Handle, 'Cant join a network without SSID, Key, and Country Code.')
    else
    begin
      if (BSSIDStr <> '') then
      begin
        ConsoleWindowWriteln(Handle, 'Using BSSID configuration ' + BSSIDStr + ' from cmdline.txt');
        bssid.octet[0] := hex2dec(copy(BSSIDStr, 1, 2));
        bssid.octet[1] := hex2dec(copy(BSSIDStr, 4, 2));
        bssid.octet[2] := hex2dec(copy(BSSIDStr, 7, 2));
        bssid.octet[3] := hex2dec(copy(BSSIDStr, 10, 2));
        bssid.octet[4] := hex2dec(copy(BSSIDStr, 13, 2));
        bssid.octet[5] := hex2dec(copy(BSSIDStr, 16, 2));
      end
      else
        ConsoleWindowWriteln(Handle, 'Letting the Cypress firmware determine the best network interface from the SSID');

      status := WirelessJoinNetwork(SSID, Key, Country, WIFIJoinBlocking, WIFIReconnectAlways, BSSID, (BSSIDStr <> ''));
      IPAddress := '0.0.0.0';
      if (status = WIFI_STATUS_SUCCESS) then
      begin

        ConsoleWindowWriteln(Handle, 'Network joined, waiting for an IP address...');

        WaitForIP;

        //DumpIP;
      end
      else
      begin
        ConsoleWindowWriteLn(Handle,'Failed to join the WIFI network. Status='+inttostr(status));
        ConsoleWindowWriteln(Handle, 'Waiting for auto retry...');

        WaitForIP;

        //DumpIP;
      end;

      // Setup a slow blink of the activity LED to give an indcation that the Pi is still alive
      ActivityLEDEnable;

      while True do
      begin
        ActivityLEDOn;
        Sleep(500);
        ActivityLEDOff;
        Sleep(500);
      end;

    end;

  except
    on e : exception do
      ConsoleWindowWriteln(Handle, 'Exception: ' + e.message + ' at ' + inttohex(longword(exceptaddr), 8));
  end;

//end.


 DECOMP:=6;
 ENCODE:=1;
 //should not be set lower than  30 which is compressiong over 1500
 //
 //		38	189.4093899116
 //		44	44.058396563
 //		50	15.9377967826
 //		54	8.6079098426
 //		58	6.0368784486
 //		60	5.5454244973

 TCP_DISTORATIO:=60;
 //FILTER 0 5/3 DWT
 //FILTER 1 9/7 DWT
 FILTER:= 0;
 COMPRESSION_RATIO := 125;
 //DIS_CR_FLG 0 COMPRESSION_RATIO
 //DIS_CR_FLG 1 TCP_DISTORATIO
 DIS_CR_FLG := 0;
 if (ENCODE = 1) then

 DrawBitmap(Window,'C:\MyBitmap.bmp',0,0,DECOMP,ENCODE,TCP_DISTORATIO,FILTER, COMPRESSION_RATIO,DIS_CR_FLG);

 if(ENCODE = 0) then
 begin
 da_x0:=0;
 da_y0:=0;
 da_x1:=2048;
 da_y1:=2048;
 ff:='t_2048.j2k';
 decom_test(da_x0,da_y0,da_x1,da_y1,ff);
 DrawBitmap(Window,'C:\test_wr.bmp',0,0,DECOMP,ENCODE,TCP_DISTORATIO,FILTER, COMPRESSION_RATIO,DIS_CR_FLG);
 end;
 {ConsoleWindowWriteLn (Handle1, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);}
 ConsoleWindowWriteLn(Handle, TimeToStr(Time));
 {-----------------------------------
 X:= 0;
 y:= 0;
 Width:= 1024;
 Height:= 1024;
  if SaveBitmap(Window,'C:\MySavedBitmap.bmp',X,Y,Width,Height,24) then
  begin
   {Output a message when the file is saved}
   GraphicsWindowDrawTextEx(Window,GraphicsWindowGetFont(Window),'Bitmap file saved successfully',260,100,COLOR_BLACK,COLOR_WHITE);

  end;
 -------------------------------------------}


 ThreadHalt(0);
end.




