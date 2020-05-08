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
 {These should be in Arrays Since there will many needed }
 S1,S2,S3,S4:String;
 PS1,PS2,PS3,PS4:PByte;
 NewIV:String;
 PNewIV:PByte;
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


  {Create a console window to show what is happening}
  RightWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,True);


 {**************************Starting Point**************************}
 ConsoleWindowWriteLn (LeftWindow, 'first block Ascii ' + 'come to dedicte ');
 ConsoleWindowWriteLn (LeftWindow, 'hex of above text ' + '636f6d6520746f206465646963746520');
 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESEncryptBlock (256bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=1;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InIVStr:='000102030405060708090A0B0C0D0E0F';
 InDataStr:='636f6d6520746f206465646963746520';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);
 ConsoleWindowWriteLn (LeftWindow, '');

 NewIV:=Actual;
 ConsoleWindowWriteLn (LeftWindow, 'NewIV will be used as IV of 2nd block ' + NewIV);

     ConsoleWindowWriteLn (LeftWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (LeftWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (LeftWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (LeftWindow, 'Data:   ' + '636f6d6520746f206465646963746520');
     ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);
  {***********************************************************************}

  {***********************************************************************}
 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESDecryptBlock (256bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=0;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InDataStr:=Actual;
 InIVStr:='000102030405060708090A0B0C0D0E0F';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);
 ConsoleWindowWriteLn (LeftWindow, 'Result: ' + Actual);
     ConsoleWindowWriteLn (LeftWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (LeftWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (LeftWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (LeftWindow, 'Data:   ' + NewIV);
     ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);
  {***********************************************************************}

{***********************************************************************}


//ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'NewIV ' +  NewIV);
 PNewIV:=@NewIV;
 StringToBytes(NewIV,PByte(PNewIV),AES_BLOCK_SIZE);
 S1:=BytesToString(PNewIV,16);
 ConsoleWindowWriteLn (LeftWindow, 'S1 ' + S1);

//ConsoleWindowWriteLn (LeftWindow, '');
ConsoleWindowWriteLn (LeftWindow, '2nd  block Ascii ' + 'a portion of the');
 ConsoleWindowWriteLn (LeftWindow, 'hex of above text ' + '6120704f7274696f6e206f6620746865');
//ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESEncryptBlock (256bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=1;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InIVStr:=S1;
 InDataStr:='6120704f7274696f6e206f6620746865';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);


     ConsoleWindowWriteLn (LeftWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (LeftWindow, 'IVector:' + S1 );
     ConsoleWindowWriteLn (LeftWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (LeftWindow, 'Data:   ' + '6120704f7274696f6e206f6620746865');
     ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);
  {***********************************************************************}

  {***********************************************************************}
  S2:=Actual;
 ConsoleWindowWriteLn (LeftWindow, 'S2 ' + S2);
 //ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'AESDecryptBlock (256bit)');
 ConsoleWindowWriteLn (LeftWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=0;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InDataStr:=S2;
 InIVStr:=S1;

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);

     ConsoleWindowWriteLn (LeftWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (LeftWindow, 'IVector:' + S1 );
     ConsoleWindowWriteLn (LeftWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (LeftWindow, 'Data:   ' + S2);
     ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + Actual);

 {**************************Starting Point**************************}
 //ConsoleWindowWriteLn (RightWindow, 'third block Ascii ' + 'come to dedicte ');
 {**************************Reversed the block**************************}
 ConsoleWindowWriteLn (RightWindow, 'third block Ascii ' + ' etcided ot emoc' );
 //ConsoleWindowWriteLn (RightWindow, 'hex of above text ' + '636f6d6520746f206465646963746520');
{**************************Reversed the block**************************}
 ConsoleWindowWriteLn (RightWindow, 'hex of above text ' + '2065746369646564206f7420656d6f63');
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESEncryptBlock (256bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=1;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InIVStr:='000102030405060708090A0B0C0D0E0F';
 InDataStr:='2065746369646564206f7420656d6f63';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);
 ConsoleWindowWriteLn (RightWindow, '');

 NewIV:=Actual;
 ConsoleWindowWriteLn (RightWindow, 'NewIV will be used as IV of 2nd block ' + NewIV);

     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + '2065746369646564206f7420656d6f63');
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}

  {***********************************************************************}
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESDecryptBlock (256bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=0;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InDataStr:=Actual;
 InIVStr:='000102030405060708090A0B0C0D0E0F';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);
 ConsoleWindowWriteLn (RightWindow, 'Result: ' + Actual);
     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + '000102030405060708090A0B0C0D0E0F' );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + NewIV);
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}

{***********************************************************************}


//ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'NewIV ' +  NewIV);
 PNewIV:=@NewIV;
 StringToBytes(NewIV,PByte(PNewIV),AES_BLOCK_SIZE);
 S1:=BytesToString(PNewIV,16);
 ConsoleWindowWriteLn (RightWindow, 'S1 ' + S1);

//ConsoleWindowWriteLn (RightWindow, '');
//ConsoleWindowWriteLn (RightWindow, '4th  block Ascii ' + 'a portion of the');
{**************************Reversed the block**************************}
 ConsoleWindowWriteLn (RightWindow, '4th  block Ascii ' + 'eht fo noitrop a');
 //ConsoleWindowWriteLn (RightWindow, 'hex of above text ' + '6120704f7274696f6e206f6620746865');
{**************************Reversed the block**************************}
 ConsoleWindowWriteLn (RightWindow, 'hex of above text ' + '65687420666f206e6f6974724f702061');
//ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESEncryptBlock (256bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=1;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InIVStr:=S1;
 InDataStr:='65687420666f206e6f6974724f702061';

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);


     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + S1 );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + '6120704f7274696f6e206f6620746865');
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}

  {***********************************************************************}
  S2:=Actual;
 ConsoleWindowWriteLn (RightWindow, 'S2 ' + S2);
 //ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'AESDecryptBlock (256bit)');
 ConsoleWindowWriteLn (RightWindow, 'Cipher Block Chaining (CBC)');

 InKey:=2;
 EncryptDecrypt:=0;
 InKeyStr:='603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4';
 InDataStr:=S2;
 InIVStr:=S1;

 Actual:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);

     ConsoleWindowWriteLn (RightWindow, 'Key:    ' + '603deb1015ca71be2b73aef0857d77811f352c073b6108d72d9810a30914dff4');
     ConsoleWindowWriteLn (RightWindow, 'IVector:' + S1 );
     ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
     ConsoleWindowWriteLn (RightWindow, 'Data:   ' + S2);
     ConsoleWindowWriteLn (RightWindow, 'Actual: ' + Actual);
  {***********************************************************************}


 {Halt this thread}
  end.

 ThreadHalt(0);
end.




