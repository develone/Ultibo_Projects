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
 RightWindow:TWindowHandle;
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

 Cipher:PCipherContext;

 key:String;
 Data:String;
 Actual:String;
 PData:PString;
 Datalen:LongWord;

 InKey:LongWord;
 InKeyStr:String;
 InDataStr:String;
 InIVStr:String;
 EncryptDecrypt:LongWord;
  function ecbencryption(InKeyStr,InDataStr:String;InKey,EncryptDecrypt:LongWord):String;
  var
  AESECBKey:PByte;
  AESECBData:PByte;
  AESECBAESKey:TAESKey;
  begin

   AESECBData:=AllocMem(AES_BLOCK_SIZE);
   if(InKey=0) then
     begin
       AESECBKey:=AllocMem(AES_KEY_SIZE128);
       StringToBytes(InKeyStr,PByte(AESECBKey),AES_KEY_SIZE128);
       StringToBytes(InDataStr,PByte(AESECBData),AES_BLOCK_SIZE);
       AESKeySetup(AESECBKey,AES_KEY_SIZE128,@AESECBAESKey);
     end;
   if(InKey=1) then
     begin
       AESECBKey:=AllocMem(AES_KEY_SIZE192);
       StringToBytes(InKeyStr,PByte(AESECBKey),AES_KEY_SIZE192);
       StringToBytes(InDataStr,PByte(AESECBData),AES_BLOCK_SIZE);
       AESKeySetup(AESECBKey,AES_KEY_SIZE192,@AESECBAESKey);
     end;
   if(InKey=2) then
     begin
       AESECBKey:=AllocMem(AES_KEY_SIZE256);
       StringToBytes(InKeyStr,PByte(AESECBKey),AES_KEY_SIZE256);
       StringToBytes(InDataStr,PByte(AESECBData),AES_BLOCK_SIZE);
       AESKeySetup(AESECBKey,AES_KEY_SIZE256,@AESECBAESKey);
     end;

 //AESECBData:=AllocMem(AES_BLOCK_SIZE);

 if(EncryptDecrypt=1) then
     begin
      AESEncryptBlock(AESECBData,AESECBData,@AESECBAESKey);
     end;

 if(EncryptDecrypt=0) then
     begin
      AESDecryptBlock(AESECBData,AESECBData,@AESECBAESKey);
     end;





 Result:=BytesToString(PByte(AESECBData),AES_BLOCK_SIZE);

 FreeMem(AESECBKey);
 FreeMem(AESECBData);

 end;
{End of function ecbencryption}
function cbcencryption(InKeyStr,InDataStr,InIVStr:String;InKey,EncryptDecrypt:LongWord):String;
  var
  AESCBCKey:PByte;
  AESCBCData:PByte;
  AESCBCVector:PByte;
  AESECBAESKey:TAESKey;
  begin

   AESECBData:=AllocMem(AES_BLOCK_SIZE);
   if(InKey=0) then
     begin
       AESCBCKey:=AllocMem(AES_KEY_SIZE128);
       StringToBytes(InKeyStr,PByte(AESCBCKey),AES_KEY_SIZE128);
       AESCBCVector:=AllocMem(AES_BLOCK_SIZE);
       StringToBytes(InIVStr,PByte(AESCBCVector),AES_BLOCK_SIZE);
       AESCBCData:=AllocMem(AES_BLOCK_SIZE);
       StringToBytes(InDataStr,PByte(AESCBCData),AES_BLOCK_SIZE);

       Cipher:=CipherCreate(CRYPTO_CIPHER_ALG_AES,PChar(AESCBCVector),PChar(AESCBCKey),AES_KEY_SIZE128);
     end;
     
   if(InKey=1) then
     begin
       AESCBCKey:=AllocMem(AES_KEY_SIZE192);
       StringToBytes(InKeyStr,PByte(AESCBCKey),AES_KEY_SIZE192);
       AESCBCVector:=AllocMem(AES_BLOCK_SIZE);
       StringToBytes(InIVStr,PByte(AESCBCVector),AES_BLOCK_SIZE);
       AESCBCData:=AllocMem(AES_BLOCK_SIZE);
       StringToBytes(InDataStr,PByte(AESCBCData),AES_BLOCK_SIZE);

       Cipher:=CipherCreate(CRYPTO_CIPHER_ALG_AES,PChar(AESCBCVector),PChar(AESCBCKey),AES_KEY_SIZE192);
     end;

   if(InKey=2) then
     begin
       AESCBCKey:=AllocMem(AES_KEY_SIZE256);
       StringToBytes(InKeyStr,PByte(AESCBCKey),AES_KEY_SIZE256);
       AESCBCVector:=AllocMem(AES_BLOCK_SIZE);
       StringToBytes(InIVStr,PByte(AESCBCVector),AES_BLOCK_SIZE);
       AESCBCData:=AllocMem(AES_BLOCK_SIZE);
       StringToBytes(InDataStr,PByte(AESCBCData),AES_BLOCK_SIZE);

       Cipher:=CipherCreate(CRYPTO_CIPHER_ALG_AES,PChar(AESCBCVector),PChar(AESCBCKey),AES_KEY_SIZE256);
     end;

 if(EncryptDecrypt=1) then
   begin
	if Cipher <> nil then
  		begin
   			if CipherEncrypt(Cipher,AESCBCData,AESCBCData,AES_BLOCK_SIZE) then
    				begin
     			             Actual:=BytesToString(AESCBCData,AES_BLOCK_SIZE);
                                      

    		                end;
    
   //CipherDestroy(Cipher);
   end;
  end;

 if(EncryptDecrypt=0) then

  begin
	if Cipher <> nil then
  		begin
   			if CipherDecrypt(Cipher,AESCBCData,AESCBCData,AES_BLOCK_SIZE) then
    				begin
     			             Actual:=BytesToString(AESCBCData,AES_BLOCK_SIZE);
                                      

    		                end;
   
  //CipherDestroy(Cipher);
  end;
  end;
  
  Result:=Actual;
  CipherDestroy(Cipher);
  end;
{End of function cbcencryption}

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

 InKey:=0;
 EncryptDecrypt:=1;
 InKeyStr:='2b7e151628aed2a6abf7158809cf4f3c';
 InDataStr:='6bc1bee22e409f96e93d7e117393172a';
 Actual:= ecbencryption(InKeyStr,InDataStr,InKey,EncryptDecrypt);

 ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +InKeyStr);
 ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +InDataStr);
 ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);




 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESDecryptBlock (128bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Electronic Codebook (ECB)');

   InKey:=0;
   EncryptDecrypt:=0;
   InKeyStr:='2b7e151628aed2a6abf7158809cf4f3c';
   InDataStr:='3ad77bb40d7a3660a89ecaf32466ef97';
   Actual:= ecbencryption(InKeyStr,InDataStr,InKey,EncryptDecrypt);

   ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +InKeyStr);
   ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +InDataStr);
   ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);

 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESEncryptBlock (192bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Electronic Codebook (ECB)');

 InKey:=1;
 EncryptDecrypt:=1;
 InKeyStr:='8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
 InDataStr:='6bc1bee22e409f96e93d7e117393172a';
 Actual:= ecbencryption(InKeyStr,InDataStr,InKey,EncryptDecrypt);

 ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +InKeyStr);
 ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +InDataStr);
 ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);


  ConsoleWindowWriteLn (LeftWindow, '');
  ConsoleWindowWriteLn (LeftWindow, 'AESDecryptBlock (192bit)');
  ConsoleWindowWriteLn (LeftWindow, 'Electronic Codebook (ECB)');

   InKey:=1;
   EncryptDecrypt:=0;
   InKeyStr:='8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
   InDataStr:='bd334f1d6e45f25ff712a214571fa5cc';
   Actual:= ecbencryption(InKeyStr,InDataStr,InKey,EncryptDecrypt);

   ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +InKeyStr);
   ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +InDataStr);
   ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);


 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESEncryptBlock (256bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Electronic Codebook (ECB)');


 InKey:=2;
 EncryptDecrypt:=1;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InDataStr:='6bc1bee22e409f96e93d7e117393172a';
 Actual:= ecbencryption(InKeyStr,InDataStr,InKey,EncryptDecrypt);

 ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +InKeyStr);
 ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +InDataStr);
 ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);

 ConsoleWindowWriteLn (LeftWindow, '');
  ConsoleWindowWriteLn (LeftWindow, 'AESDecryptBlock (256bit)');
  ConsoleWindowWriteLn (LeftWindow, 'Electronic Codebook (ECB)');

   InKey:=2;
   EncryptDecrypt:=0;
   InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
   InDataStr:='f3eed1bdb5d2a03c064b5a7e3db181f8';
   Actual:= ecbencryption(InKeyStr,InDataStr,InKey,EncryptDecrypt);

   ConsoleWindowWriteLn (LeftWindow, 'Key:    ' +InKeyStr);
   ConsoleWindowWriteLn (LeftWindow, 'Data:   ' +InDataStr);
   ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);

  {Create a console window to show what is happening}
  RightWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,True);

  {***********************************************************************}
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESEncryptBlock (128bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=0;
 EncryptDecrypt:=1;
 InKeyStr:='2b7e151628aed2a6abf7158809cf4f3c';
 InIVStr:='000102030405060708090A0B0C0D0E0F';
 InDataStr:='6bc1bee22e409f96e93d7e117393172a';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);


     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '2b7e151628aed2a6abf7158809cf4f3c');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + '6bc1bee22e409f96e93d7e117393172a');
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}

  {***********************************************************************}
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESDecryptBlock (128bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=0;
 EncryptDecrypt:=0;
 InKeyStr:='2b7e151628aed2a6abf7158809cf4f3c';
 InDataStr:='7649abac8119b246cee98e9b12e9197d';
 InIVStr:='000102030405060708090A0B0C0D0E0F';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);

     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '2b7e151628aed2a6abf7158809cf4f3c');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + '7649abac8119b246cee98e9b12e9197d');
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}

  {***********************************************************************}
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESEncryptBlock (192bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=1;
 EncryptDecrypt:=1;
 InKeyStr:='8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
 InIVStr:='000102030405060708090A0B0C0D0E0F';
 InDataStr:='6bc1bee22e409f96e93d7e117393172a';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);


     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + '6bc1bee22e409f96e93d7e117393172a');
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}

  {***********************************************************************}
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESDecryptBlock (192bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=1;
 EncryptDecrypt:=0;
 InKeyStr:='8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b';
 InDataStr:='4f021db243bc633d7178183a9fa071e8';
 InIVStr:='000102030405060708090A0B0C0D0E0F';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);
 ConsoleWindowWriteLn (RightWindow, 'Result: ' + Actual);
     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '8e73b0f7da0e6452c810f32b809079e562f8ead2522c6b7b');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + '4f021db243bc633d7178183a9fa071e8');
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}

 {***********************************************************************}
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESEncryptBlock (256bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=1;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InIVStr:='000102030405060708090A0B0C0D0E0F';
 InDataStr:='6bc1bee22e409f96e93d7e117393172a';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);


     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + '6bc1bee22e409f96e93d7e117393172a');
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}

  {***********************************************************************}
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESDecryptBlock (256bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=0;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InDataStr:='f58c4c04d6e5f1ba779eabfb5f7bfbd6';
 InIVStr:='000102030405060708090A0B0C0D0E0F';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);
 ConsoleWindowWriteLn (RightWindow, 'Result: ' + Actual);
     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + 'f58c4c04d6e5f1ba779eabfb5f7bfbd6');
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}



 {Halt this thread}
  end.

 ThreadHalt(0);
end.




