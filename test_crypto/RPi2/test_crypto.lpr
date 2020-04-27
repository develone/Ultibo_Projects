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
  Crypto,

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

 {Pointer to HashContext}
 myPContext:^PHashContext;
 myTHashContext : THashContext;
 myAlgorithm:LongWord;
 myKeySize:LongWord;

 {Pointer to TAESKEY record
 Rounds:LongWord;
  EncryptKey:array[0..59] of LongWord;
  DecryptKey:array[0..59] of LongWord;
  }

 {Pointer to TAESKey}
 myPAESKey:^TAESKey;

 {Handle to TAESKEY record }
 myTAESKey:TAESKey;

 {Pointer to  CipherContext}
 myPCipherContext:PCipherContext;

 {Handle to TCipherContext record }
 myTCipherContext:TCipherContext;

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
 ConsoleWindowWriteLn(LeftWindow,'Starting TFTP_Template example');
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
 CryptoInit;
 {Cipher algorithms
 CRYPTO_CIPHER_ALG_NONE = 0;
 CRYPTO_CIPHER_ALG_AES  = 1;
 CRYPTO_CIPHER_ALG_DES  = 2;
 CRYPTO_CIPHER_ALG_3DES = 3;
 CRYPTO_CIPHER_ALG_RC4  = 4;}


 myAlgorithm:=1;
 {Cipher algorithm  CRYPTO_CIPHER_ALG_AES
 defined in crypto.pas 0 to 4}
 ConsoleWindowWriteLn (LeftWindow, 'Cipher Algorithm  CRYPTO_CIPHER_ALG_AES ' + intToStr(myAlgorithm));

 myPCipherContext := @myTCipherContext;
 ConsoleWindowWriteLn (LeftWindow, 'myPCipherContext Pointer ' + HexStr(myPCipherContext));
 myTCipherContext.Algorithm := myAlgorithm;
 ConsoleWindowWriteLn (LeftWindow, 'myPCipherContext.Algorithm ' + intToStr(myTCipherContext.Algorithm));
 myKeySize := 59;
 ConsoleWindowWriteLn (LeftWindow, 'array 0..59 ' + intToStr(myKeySize));

 myPAESKey := @myTAESKey;
 ConsoleWindowWriteLn (LeftWindow, 'myPAESKey Pointer ' + HexStr(myPAESKey));
 myTAESKey.Rounds:=14;
 ConsoleWindowWriteLn (LeftWindow, 'myTAESKey.Rounds ' + intToStr(myTAESKey.Rounds));

 //myPContext :=  HashCreate(myAlgorithm;myPKEYAES;KeySize:LongWord);
 {Halt this thread}
 ThreadHalt(0);
end.




