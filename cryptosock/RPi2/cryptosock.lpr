program cryptosock;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi2,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,

  Logging,
  SysUtils, { TimeToStr & Time }
  Classes,
  Ultibo,
  syncobjs,
  blcksock,
  synsock,
  crypto,
  BCM2836,
  BCM2709,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  uTFTP,
  Winsock2,
  { needed for telnet }
  Shell,
  ShellFilesystem,
  ShellUpdate,
  RemoteShell
  { needed for telnet }


  { Add additional units here };
 var
  MyKey: AnsiString = '1234567890123456'; {Must be 16, 24 or 32 bytes}
  MyIV: AnsiString = 'My Secret IV';
  MyAAD: AnsiString = 'My Extra Secret AAD';
  MyData: AnsiString = 'The quick brown fox jumps over the lazy dog.The quick brown fox jumps over the lazy dog.';
  MyResult: AnsiString;
  Key: PByte;
  IV: PByte;
  AAD: PByte;
  Plain: PByte;
  Crypt: PByte;
  Tag: PByte;
  LeftWindow:TWindowHandle;
  //RightWindow:TWindowHandle;
  HTTPListener:THTTPListener;
  { needed to use ultibo-tftp  }
  TCP : TWinsock2TCPClient;
  IPAddress : string;

   {End of function disresults}
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

   ConsoleWindowWriteLn (LeftWindow, s);

 end;



 procedure WaitForSDDrive;

 begin

   while not DirectoryExists ('C:\') do sleep (500);

 end;
begin
  {Create a console window to show what is happening}

  { Add your program code here }


 {The following 3 lines are logging to the console
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
 }

 {The following 2 lines are logging to a file
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE)); }

  LeftWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);

  {Display a startup message on the console}
 ConsoleWindowWriteLn(LeftWindow,'Starting TFTP_test_crypto example');
  // wait for IP address and SD Card to be initialised.
 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 ConsoleWindowWriteLn (LeftWindow, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);

 //HTTPListener.RegisterDocument('',TWebStatusAPICrypto.Create);


  ConsoleWindowWriteLn(LeftWindow, 'AES GCM Encrypt/Decrypt test');
  ConsoleWindowWriteLn(LeftWindow, 'MyData is ' + MyData);

  {Allocate buffers}
  Key := AllocMem(Length(MyKey));
  IV := AllocMem(Length(MyIV));
  AAD := AllocMem(Length(MyAAD));
  Plain := AllocMem(Length(MyData));
  Crypt := AllocMem(Length(MyData));
  Tag := AllocMem(AES_BLOCK_SIZE);

  {Copy the values}
  Move(MyKey[1], Key^, Length(MyKey));
  Move(MyIV[1], IV^, Length(MyIV));
  Move(MyAAD[1], AAD^, Length(MyAAD));
  Move(MyData[1], Plain^, Length(MyData));

  {Clear the crypt buffer}
  FillChar(Crypt^, Length(MyData), 0);

  {Encrypt the data}
  if AESGCMEncryptData(Key, Length(MyKey), IV, AAD, Plain, Crypt, Length(MyIV), Length(MyAAD), Length(MyData), Tag) then
  begin
    ConsoleWindowWriteLn(LeftWindow, 'AES GCM Encrypt Success');

    {Clear the plain buffer}
    FillChar(Plain^, Length(MyData), 0);

    {Decrypt the Data}
    if AESGCMDecryptData(Key, Length(MyKey), IV, AAD, Crypt, Plain, Length(MyIV), Length(MyAAD), Length(MyData), Tag) then
    begin
      ConsoleWindowWriteLn(LeftWindow, 'AES GCM Decrypt Success');

      {Copy the result}
      SetString(MyResult, PAnsiChar(Plain), Length(MyData));

      ConsoleWindowWriteLn(LeftWindow, 'MyResult is ' + MyResult);
    end
    else
    begin
      ConsoleWindowWriteLn(LeftWindow, 'AES GCM Decrypt Failure');
    end;
  end
  else
  begin
    ConsoleWindowWriteLn(LeftWindow, 'AES GCM Encrypt Failure');
  end;

    ThreadHalt(0);
   end.

