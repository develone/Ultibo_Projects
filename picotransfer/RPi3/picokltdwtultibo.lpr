program picokltdwtultibo ;

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
  Serial,   {Include the Serial unit so we can open, read and write to the device}
  USBCDCACM,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  uTFTP,
  Winsock2,
  { needed to use ultibo-tftp  }
  { needed for telnet }
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell,
  { needed for telnet }
  Syscalls,
  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  uReadBin,
  Classes,
  MMC;         {Include the MMC/SD core to access our SD card}



const
  stUnknown = 0;
  stFind    = 1;
  stOpen    = 2;
  stLedonoff  = 3;
  stProcess0  = 4;
  stProcess1  = 5;
  stProcess2  = 6;
  stClose   = 7;
  stClosed  = 8;
 type
 StrBuffer = array[0..65] of String;
 StrBufPtr = ^StrBuffer;
var
  Count : LongWord;
  ch : char;
  Character : char;
  Characters : string;
  PCharacters : PString;
  WindowHandle : TWindowHandle;
  SerialDevice : PSerialDevice;

  res : LongWord;


  state, oldState : integer;
  s : string;

  HTTPListener:THTTPListener;
  IPAddress : string;
  MyPLoggingDevice : ^TLoggingDevice;
  LogDevice : PLoggingDevice;
  aFilename:String;
  aStringList:TStringList;
  StrB : StrBuffer;
 StrBP : StrBufPtr;
 PP : Pointer;

 procedure Log (s : string);
begin
  ConsoleWindowWriteLn (WindowHandle, s);
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



procedure Msg (Sender : TObject; s : string);

begin

  ConsoleWindowWriteLn (WindowHandle, s);

end;



procedure WaitForSDDrive;

begin

  while not DirectoryExists ('C:\') do sleep (500);

end;


begin

  LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\pico.log');
  LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));
  MyPLoggingDevice:=LoggingDeviceGetDefault;
  LoggingDeviceRedirectOutput(MyPLoggingDevice);

  WindowHandle := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_FULL, true);

  WaitForSDDrive;
  IPAddress := WaitForIPComplete;
  {Create and start the HTTP Listener for our web status page}
  HTTPListener:=THTTPListener.Create;
  HTTPListener.Active:=True;
  Sleep(5000);
  {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
  WebStatusRegister(HTTPListener,'','',True);

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
            stLedonoff  : s := 'Ledonoff';
            stProcess0  : s := 'Processing0';
            stProcess1  : s := 'Processing1';
            stProcess2  : s := 'Processing2';
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
            ConsoleWindowWriteLn (WindowHandle, 'trying to find USB CDC ACM Serial');
            ConsoleWindowWriteLn (WindowHandle, 'testing reading a file');
            aFilename:='C:\bb.bin';
            setFileName(aFileName);
            SetStingList(aStringList);
            StrBP:=@PP;
            i:=0;
            Characters:=Readit(i);
            while (i < 4160) do
    	    begin

                 Characters:=ReadBuffer(i);
                 PCharacters:=@Characters;
                 //StrBP^:=PCharacters^;
                 ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+Characters);
                 Inc(PCharacters);
                 i:=i+65;
            end;

            if SerialDevice <> nil then state := stOpen;
          end;
        stOpen :
          begin

            sleep(2000);
             res := SerialDeviceOpen (SerialDevice, 115200, SERIAL_DATA_8BIT, SERIAL_STOP_1BIT, SERIAL_PARITY_NONE, SERIAL_FLOW_DSR_DTR, 0, 0);
             //characters := 'You said ' + Characters + #13#10;
             //SerialDeviceWrite (SerialDevice, PChar (Characters),Length (Characters), SERIAL_WRITE_NONE, Count);
             //Characters:='';
            //res := SerialDeviceOpen (SerialDevice, 9600, SERIAL_DATA_8BIT, SERIAL_STOP_1BIT, SERIAL_PARITY_NONE, SERIAL_FLOW_NONE, 0, 0);
            if res = ERROR_SUCCESS then state := stProcess0
            else if res = ERROR_INVALID_PARAMETER then state := stFind;
          end;

        stLedonoff :

          	while(True) do
		begin
			characters := '1';
			Count := 1;
                        ConsoleWindowWriteLn (WindowHandle, 'turning on led');
			SerialDeviceWrite (SerialDevice, PChar (Characters),Length (Characters), SERIAL_WRITE_NONE, Count);

			Sleep(5000);
			characters := '0';
                        ConsoleWindowWriteLn (WindowHandle, 'turning off led');
			SerialDeviceWrite (SerialDevice, PChar (Characters),Length (Characters), SERIAL_WRITE_NONE, Count);

			Sleep(5000);
		end;
        stProcess0 :
          begin
            res := SerialDeviceRead (SerialDevice, @ch, SizeOf (ch), SERIAL_READ_NON_BLOCK, Count);
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
                if SerialDevice <> nil then SerialDeviceClose (SerialDevice);
                SerialDevice := nil;
                state := stFind;
              end;
          end;

         stProcess1 :
                   begin

                   end;
         stProcess2 :
                    begin

                    end;
        end; // case
      sleep (100);
    end; // true;
  { Halt the thread if we exit the loop }
  ThreadHalt (0);
end.


