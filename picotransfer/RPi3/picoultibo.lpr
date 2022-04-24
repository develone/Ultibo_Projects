program picoultibo ;

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
  stProcess3  = 7;
  stProcess4  = 8;
  stProcess5  = 9;
  stClose   = 10;
  stClosed  = 11;
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
  WrFilename:String;
  WrStringList:TStringList;
  WrFileStream:TFileStream;
  StrB : StrBuffer;
 StrBP : StrBufPtr;
 syncflag : integer;
 readyflag : integer;
 cmdflag : integer;
 numberoflinestowrite: integer;
 debug_stprocces1 : boolean;
 debug_stprocces2 : boolean;
 debug_stprocces3 : boolean;
 debug_stprocces4 : boolean;
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
  syncflag:=0;
  readyflag:=0;
  cmdflag:=0;
  debug_stprocces1:=False;
  debug_stprocces2:=False;
  debug_stprocces3:=False;
  debug_stprocces4:=False;
  WrFileName:='C:\kltdwt.txt';
  numberoflinestowrite:=0;
  //try
  //WrFileStream:=TFileStream.Create(WrFilename,fmCreate);

  {We've created the file, now we need to write some content to it, we can use
  a TStringList for that but there are many other ways as well.}
  //WrStringList:=TStringList.Create;

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
  Sleep(2000);
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
            stProcess3  : s := 'Processing3';
            stProcess4  : s := 'Processing4';
            stProcess5  : s := 'Processing5';
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
            {ConsoleWindowWriteLn (WindowHandle, 'testing reading a file');
            aFilename:='C:\bb.bin';
            setFileName(aFileName);
            SetStingList(aStringList);

            StrBP:=@StrB;
            i:=0;
            Characters:=Readit(i);
            while (i < 4160) do
    	    begin

                 Characters:=ReadBuffer(i);
                 PCharacters:=@Characters;

                 StrBP^[j]:=Characters;
                 ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+StrBP^[j]);
                 Inc(PCharacters);
                 i:=i+65;
                 Inc(j);
            end;
            }
            if SerialDevice <> nil then state := stOpen;
          end;
        stOpen :
          begin
            sleep(1000);
             res := SerialDeviceOpen (SerialDevice, 115200, SERIAL_DATA_8BIT, SERIAL_STOP_1BIT, SERIAL_PARITY_NONE, SERIAL_FLOW_DSR_DTR, 0, 0);
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
            begin {level0}
            res := SerialDeviceRead (SerialDevice, @ch, SizeOf (ch), SERIAL_READ_NON_BLOCK, Count);
            if (res = ERROR_SUCCESS) and (Count > 0) then  // non blocking so count may be 0
              begin
                Log ('Received a line: "' + Display (Characters) + '"');
                //begin //<-------
                if ((ch = #13) or (ch =#10)) then
                  begin {level2}

                    ConsoleWindowWriteLn (WindowHandle,'Length of Characters '+intToStr(Length(Characters)));
                    if(syncflag=0) then
                      begin {level3}
                        if( Length(Characters)=4) and (Characters[1]='S') then
                          begin {level4}
                            syncflag:=1;
                            ConsoleWindowWriteLn (WindowHandle,'Characters '+Characters[1]);
                            Characters := '';
                            state:= stProcess1;
                          end {level4}
                        else
                         if(Length(Characters)>4) then
                           begin
                             Characters := '';
                           end;
                      end; {level3}  //<-------
                  end {level2} //<-------
                  //end {level1} //<-------
                else
                  Characters := Characters + ch;
                //end;
              end //<-------
            else if res = ERROR_INVALID_PARAMETER then
                  begin
                    if SerialDevice <> nil then SerialDeviceClose (SerialDevice);
                      begin
                        SerialDevice := nil;
                        state := stFind;
                      end
                  end




           end;

        stProcess1 :
                begin {level0}
                while (debug_stprocces1) do
                begin
                end;
                res := SerialDeviceRead (SerialDevice, @ch, SizeOf (ch), SERIAL_READ_NON_BLOCK, Count);
                if (res = ERROR_SUCCESS) and (Count > 0) then  // non blocking so count may be 0
                  begin
                    Log ('Received a line: "' + Display (Characters) + '"');
                    //begin //<-------
                    if ((ch = #13) or (ch =#10)) then
                      begin {level2}

                        ConsoleWindowWriteLn (WindowHandle,'Length of Characters '+intToStr(Length(Characters)));
                        if(readyflag=0) then
                          begin {level3}

                            if( Length(Characters)=5) and (Characters[1]='R') then
                              begin {level4}
                                readyflag:=1;
                                ConsoleWindowWriteLn (WindowHandle,'Characters '+Characters[1]);
                                Characters := '';
                                sleep(8000);
                                SerialDeviceFlush(SerialDevice,SERIAL_WRITE_NONE);
                                state:= stProcess2;
                              end {level4}
                            else
                              if(Length(characters)>5) then
                                begin
                                 Characters := '';

                                end;
                          end; {level3} //<-------
                      end {level2} //<-------
                      //end {level1} //<-------
                    else
                      Characters := Characters + ch;
                    //end;
                  end //<-------
                else if res = ERROR_INVALID_PARAMETER then
                      begin
                        if SerialDevice <> nil then SerialDeviceClose (SerialDevice);
                          begin
                            SerialDevice := nil;
                            state := stFind;
                          end
                      end
                end;
        stProcess2 :
          begin
          //sleep(8500);
          //SerialDeviceFlush(SerialDevice,SERIAL_WRITE_NONE);
          while (debug_stprocces2) do
                begin
                end;
          //ConsoleWindowWriteLn (WindowHandle,'sleeping for 20 sec');
          sleep(100);
          //SerialDeviceFlush(SerialDevice,SERIAL_WRITE_NONE);
          aFilename:='C:\bb.bin';
          setFileName(aFileName);
          SetStingList(aStringList);

          StrBP:=@StrB;
          i:=0;
          Characters:=Readit(i);
          while (i < 4160) do
    	    begin

                 Characters:=ReadBuffer(i);
                 PCharacters:=@Characters;

                 StrBP^[j]:=Characters;

                 ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+StrBP^[j]);
                 Characters:=StrBP^[j];
                 Count := 65;
                 SerialDeviceWrite (SerialDevice, PChar (Characters),Length (Characters), SERIAL_WRITE_NONE, Count);
                 //sleep(500);
                 {
                 Characters:=#13+#10;
                 Count := 2;
                 SerialDeviceWrite (SerialDevice, PChar (Characters),Length (Characters), SERIAL_WRITE_NONE, Count);
                 res := SerialDeviceRead (SerialDevice, @ch, SizeOf (ch), SERIAL_READ_NON_BLOCK, Count);
                 if (res = ERROR_SUCCESS) and (Count > 0) then  // non blocking so count may be 0
                    begin
                         Log ('Received a line: "' + Display (Characters) + '"');
                         //begin //<-------
                         if ((ch = #13) or (ch =#10)) then
                         begin {level2}

                               ConsoleWindowWriteLn (WindowHandle,'Length of Characters '+intToStr(Length(Characters)));
                         end;
                    end;
                  }
                 //ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+StrBP^[j]);
                 Inc(PCharacters);
                 i:=i+65;
                 Inc(j);
            end;
            //SerialDeviceFlush(SerialDevice,SERIAL_WRITE_NONE);


            state:= stProcess3;
            {characters := '1';
	    Count := 1;
            SerialDeviceWrite (SerialDevice, PChar (Characters),Length (Characters), SERIAL_WRITE_NONE, Count);
            sleep(500);
            res := SerialDeviceRead (SerialDevice, @ch, SizeOf (ch), SERIAL_READ_NON_BLOCK, Count);
            if (res = ERROR_SUCCESS) and (Count > 0) then  // non blocking so count may be 0
              begin
                Log ('Received a line: "' + Display (Characters) + '"');
                //begin //<-------
                if ((ch = #13) or (ch =#10)) then
                  begin {level2}

                    ConsoleWindowWriteLn (WindowHandle,'Length of Characters '+intToStr(Length(Characters)));
                  end;
              end;
             }

          end;
        stProcess3 :
            begin {level0}

            while (debug_stprocces3) do
                begin
                end;
            res := SerialDeviceRead (SerialDevice, @ch, SizeOf (ch), SERIAL_READ_NON_BLOCK, Count);
            if (res = ERROR_SUCCESS) and (Count > 0) then  // non blocking so count may be 0
              begin
                Log ('Received a line: "' + Display (Characters) + '"');
                //begin //<-------
                if ((ch = #13) or (ch =#10)) then
                  begin {level2}

                    ConsoleWindowWriteLn (WindowHandle,'Length of Characters '+intToStr(Length(Characters)));
                    if(cmdflag=0) then
                      begin {level3}
                        if( Length(Characters)=36) and (Characters='ReadyCommand (1 = Send or 0 = Wait):') then
                          begin {level4}
                            syncflag:=1;
                            ConsoleWindowWriteLn (WindowHandle,'Characters '+Characters[1]);
                            Characters := '';
                            state:= stProcess4;
                          end {level4}
                        else
                         if(Length(Characters)>31) then
                           begin
                             Characters := '';
                           end;
                      end; {level3}  //<-------
                  end {level2} //<-------
                  //end {level1} //<-------
                else
                  Characters := Characters + ch;
                //end;
              end //<-------
            else if res = ERROR_INVALID_PARAMETER then
                  begin
                    if SerialDevice <> nil then SerialDeviceClose (SerialDevice);
                      begin
                        SerialDevice := nil;
                        state := stFind;
                      end
                  end
             end;
        stProcess4 :
            begin
            while (debug_stprocces4) do
                begin
                end;
                   characters := '1';
		   Count := 1;
                   ConsoleWindowWriteLn (WindowHandle, 'turning on led');
		   SerialDeviceWrite (SerialDevice, PChar (Characters),Length (Characters), SERIAL_WRITE_NONE, Count);

                   state := stProcess5;
            end;
        stProcess5 :
            begin
            res := SerialDeviceRead (SerialDevice, @ch, SizeOf (ch), SERIAL_READ_NON_BLOCK, Count);
            if (res = ERROR_SUCCESS) and (Count > 0) then  // non blocking so count may be 0
              begin
                if ((ch = #13) or (ch =#10)) then
                  begin
                    Log ('Received a line: "' + Display (Characters) + '"');
                    {
                    if (numberoflinestowrite<100) then
                      begin
                        WrStringList.Add(Characters);
                        Inc(numberoflinestowrite);
                      end
                      else
                      begin
                         WrFileStream.Free;
                         WrStringList.Free;
                      end;
                    }
                    //sleep(100);
                    Characters := '';
                  end
                else
                  Characters := Characters + ch;
              end
            else if res = ERROR_INVALID_PARAMETER then
              begin
                if SerialDevice <> nil then SerialDeviceClose (SerialDevice);
                SerialDevice := nil;

                state := stProcess0;
              end;
          end;
        end; // case
    end; // true;
  { Halt the thread if we exit the loop }
  ThreadHalt (0);
end.



