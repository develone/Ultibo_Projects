program test_crypto;

{$mode objfpc}{$H+}

{ Advanced example - Template                                             }
{                                                                         }
{ This example shows how to create  webserver and TFTP server with remote shell  }
{ This version is for Raspberry Pi 2B and will also work on a 3B.                }

{ After the first time that kernel7.img has been transferred to micro sd card    }
{ tftp xx.xx.xx.xx < cmdstftp                                                    }
{ contents of cmdstftp                                                           }
{ binary                                                                         }
{ put kernel7.img                                                                }
{ quit                                                                           }

uses
  {InitUnit,     Include InitUnit to allow us to change the startup behaviour}
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Threads,
  Console,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  SysUtils,  { TimeToStr & Time }
  Logging,
  uTFTP,
  Winsock2,
  Platform,
  Crypto,
  APICrypto,

  { needed to use ultibo-tftp  }
  { needed for telnet }
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell;
  { needed for telnet }

var
 LeftWindow:TWindowHandle;
 HTTPListener:THTTPListener;
 { needed to use ultibo-tftp  }
 TCP : TWinsock2TCPClient;
 IPAddress : string;

 AESECBKey:PByte;
 AESECBData:PByte;
 AESECBAESKey:TAESKey;

 AESCBCKey:PByte;
 AESCBCData:PByte;
 AESCBCVector:PByte;
 //Context:PBigIntContext;
 Cipher:PCipherContext;

 key:String;
 Data:String;
 Actual:String;
 PData:PString;
 Datalen:LongWord;

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
 {The following 3 lines are logging to the console
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
 }

 {The following 2 lines are logging to a file
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE)); }


 {Create a console window to show what is happening}
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

 HTTPListener.RegisterDocument('',TWebStatusAPICrypto.Create);

 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESEncryptBlock (128bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Electronic Codebook (ECB)');
 AESECBKey:=AllocMem(AES_KEY_SIZE128);
 StringToBytes('2b7e151628aed2a6abf7158809cf4f3c',PByte(AESECBKey),AES_KEY_SIZE128);
 AESECBData:=AllocMem(AES_BLOCK_SIZE);
 StringToBytes('6bc1bee22e409f96e93d7e117393172a',PByte(AESECBData),AES_BLOCK_SIZE);
 AESKeySetup(AESECBKey,AES_KEY_SIZE128,@AESECBAESKey);
 AESEncryptBlock(AESECBData,AESECBData,@AESECBAESKey);
 Actual:=BytesToString(PByte(AESECBData),AES_BLOCK_SIZE);
 ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +'2b7e151628aed2a6abf7158809cf4f3c');
 ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +'6bc1bee22e409f96e93d7e117393172a');
 ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);
 FreeMem(AESECBKey);
 FreeMem(AESECBData);

 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESEncryptBlock (192bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Electronic Codebook (ECB)');
 AESECBKey:=AllocMem(AES_KEY_SIZE192);
 StringToBytes('8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b',PByte(AESECBKey),AES_KEY_SIZE192);
 AESECBData:=AllocMem(AES_BLOCK_SIZE);
 StringToBytes('6bc1bee22e409f96e93d7e117393172a',PByte(AESECBData),AES_BLOCK_SIZE);
 AESKeySetup(AESECBKey,AES_KEY_SIZE192,@AESECBAESKey);
 AESEncryptBlock(AESECBData,AESECBData,@AESECBAESKey);
 Actual:=BytesToString(PByte(AESECBData),AES_BLOCK_SIZE);
 ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +'8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b');
 ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +'6bc1bee22e409f96e93d7e117393172a');
 ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);
 FreeMem(AESECBKey);
 FreeMem(AESECBData);

 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESEncryptBlock (256bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Electronic Codebook (ECB)');
 AESECBKey:=AllocMem(AES_KEY_SIZE256);
 StringToBytes('603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4',PByte(AESECBKey),AES_KEY_SIZE256);
 AESECBData:=AllocMem(AES_BLOCK_SIZE);
 StringToBytes('6bc1bee22e409f96e93d7e117393172a',PByte(AESECBData),AES_BLOCK_SIZE);
 AESKeySetup(AESECBKey,AES_KEY_SIZE256,@AESECBAESKey);
 AESEncryptBlock(AESECBData,AESECBData,@AESECBAESKey);
 Actual:=BytesToString(PByte(AESECBData),AES_BLOCK_SIZE);
 ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +'603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
 ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +'6bc1bee22e409f96e93d7e117393172a');
 ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);
 FreeMem(AESECBKey);
 FreeMem(AESECBData);


 {Halt this thread}

 ThreadHalt(0);
end.




