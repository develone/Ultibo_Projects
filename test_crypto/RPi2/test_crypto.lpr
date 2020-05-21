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
       Classes,     {Include the common classes}
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell,
  { needed for telnet }
  BCM2836,
     FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC,         {Include the MMC/SD core to access our SD card}
  BCM2709;     {And also include the MMC/SD driver for the Raspberry Pi}

{
function BytesToString(Data:PByte;Size:LongWord):String;
function StringToBytes(const Value:String;Data:PByte;Size:LongWord):Boolean;
}
type

CBC = record
  {0123456789abcdef0123456789abcdef}
  StrKeyAsc:String[32];
  {0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef}
  StrKeyHex:String[80];
  {0123456789abcdef}
  Strplaintext:array [1..1024] of String[16];
  StrIV:array [1..1024] of String[32];
  StrEnc:array [1..1024] of String[32];
  StrDec:array [1..1024] of String[32];
end;
GCM = record
  {128Bit Key}
   
  {Now we are engag}
  StrKeyAsc:array [1..32] of String[16];
  {01234567890123456789012345678901
   4e6f772077652061726520656e676167}
  StrKeyHex:array [1..32] of String[32];
  Strplaintext:array [1..32] of String[16];
  StrData:array [1..32] of String[32];
  StrEnc:array [1..32] of String[32];
  StrDec:array [1..32] of String[32];
  StrIV:String[32];
  
  StrAAD: String[32];
  Actual:array [1..32] of String[80];
  Expected:array [1..32] of String[80];
  ActualTag:array [1..32] of String[80];
  ExpectedTag:array [1..32] of String[80];
end;
 var
  PCBC:^CBC;
  PGCM:^GCM;
  CBC1:CBC;
  GCM1:GCM;
 
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

 AESGCMKey:PByte;
 AESGCMIV:PByte;
 AESGCMAAD:PByte;
 AESGCMData:PByte;
 AESGCMTag:PByte;

 Cipher:PCipherContext;


 Actual:String;

 PStrIV64:^Char;
 InKey:LongWord;
 InKeyStr:String;
 InDataStr:String;
 InIVStr:String;
 EncryptDecrypt:LongWord;
 start,stop,lftrht:LongWord;
 {These should be in Arrays Since there will many needed }
 S1,S2:String;

 Count:Integer;
 Filename:String;
 StringList:TStringList;
 FileStream:TFileStream;
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
function encrecord(start,stop:LongWord):boolean;
var
 LP:LongWord;
begin

 for LP:=start to stop do
 begin
 InKey:=2;
 EncryptDecrypt:=1;
 InKeyStr:=CBC1.StrKeyHex;
 InIVStr:=CBC1.StrIV[LP];
 S1:=CBC1.Strplaintext[LP];
 S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
 InDataStr:=S2;

 CBC1.StrEnc[LP]:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);
 {Encrypted of previous blk bec IVector of next blk}
 CBC1.StrIV[LP+1]:=CBC1.StrEnc[LP];

 end;
 
 
 
for LP:=start to stop do
 begin
 InKey:=2;
 EncryptDecrypt:=0;
 InKeyStr:=CBC1.StrKeyHex;
 InIVStr:=CBC1.StrIV[LP];

 InDataStr:=CBC1.StrEnc[LP];

 CBC1.StrDec[LP]:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);

 end;
 Result:=True;
 end;
{End of function encrecord}
function disresults(start,stop,lfrht:longWord):boolean;
var
 LP:LongWord;
begin




 for LP:=start to stop do
 begin

 if(lftrht=0) then
 begin
 ConsoleWindowWriteLn (LeftWindow, '');
 ConsoleWindowWriteLn (LeftWindow, 'Key:    ' + CBC1.StrKeyHex);
 ConsoleWindowWriteLn (LeftWindow, 'IVector:' + CBC1.StrIV[LP] );
 ConsoleWindowWriteLn (LeftWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
 ConsoleWindowWriteLn (LeftWindow, 'Data:   ' + CBC1.StrEnc[LP]);
 ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + CBC1.StrDec[LP]);
 end;
 if(lftrht=1) then
 begin
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'Key:    ' + CBC1.StrKeyHex);
 ConsoleWindowWriteLn (RightWindow, 'IVector:' + CBC1.StrIV[LP] );
 ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
 ConsoleWindowWriteLn (RightWindow, 'Data:   ' + CBC1.StrEnc[LP]);
 ConsoleWindowWriteLn (RightWindow, 'Actual: ' + CBC1.StrDec[LP]);
 end;
  Result:=True;
 end;
 end;
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
 Filename:='C:\Record Data File Handling.txt';
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

{**************************Start Init**************************}
  {Create the record}
  
  CBC1.StrIV[0]:='000102030405060708090A0B0C0D0E0F';
                       {0123456789abcdef0123456789abcdef}
  CBC1.StrKeyAsc:='Now we are engaged in a great ci';
  S1:=CBC1.StrKeyAsc;
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  CBC1.StrKeyHex:=S2;
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.StrKeyAsc ' + CBC1.StrKeyAsc);
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.StrKeyHex ' + CBC1.StrKeyHex);

  CBC1.Strplaintext[0]:='Four score and s';

  CBC1.Strplaintext[1]:='even years ago o';

  CBC1.Strplaintext[2]:='ur fathers broug';

  CBC1.Strplaintext[3]:='ht forth on this';

  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[0] ' + CBC1.Strplaintext[0]);
  S1:=CBC1.Strplaintext[0];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[0] ' + S2);

  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[1] ' + CBC1.Strplaintext[1]);
  S1:=CBC1.Strplaintext[1];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[1] ' + S2);

  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[2] ' + CBC1.Strplaintext[2]);
  S1:=CBC1.Strplaintext[2];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[2] ' + S2);

  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[3] ' + CBC1.Strplaintext[3]);
  S1:=CBC1.Strplaintext[3];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[3] ' + S2);
{**************************End Init**************************}


 start:=0;
 stop:=3;
 encrecord(start,stop);
 lftrht:=0;
 disresults(start,stop,lftrht);


 {**************************Start Init4 to 7**************************}
  ConsoleWindowWriteLn (LeftWindow, '');
  CBC1.Strplaintext[4]:=' continent, a ne';

  CBC1.Strplaintext[5]:='w nation, concei';

  CBC1.Strplaintext[6]:='ved in Liberty, ';

  CBC1.Strplaintext[7]:='and dedicated to';

  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[4] ' + CBC1.Strplaintext[4]);
  S1:=CBC1.Strplaintext[4];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[4] ' + S2);
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[5] ' + CBC1.Strplaintext[5]);
  S1:=CBC1.Strplaintext[5];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[5] ' + S2);
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[6] ' + CBC1.Strplaintext[6]);
  S1:=CBC1.Strplaintext[6];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[6] ' + S2);
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[7] ' + CBC1.Strplaintext[7]);
  S1:=CBC1.Strplaintext[7];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'CBC1.Strplaintext[7] ' + S2);

  start:=4;
 stop:=7;
 encrecord(start,stop);
 lftrht:=1;
 disresults(start,stop,lftrht);
 {ConsoleWindowWriteLn(RightWindow,'Length ' + IntToStr(length(CBC1.StrKeyHex)));
 ConsoleWindowWriteLn(RightWindow,'PWKGDATA ' + HexStr(PWKGDATA));
 ConsoleWindowWriteLn(RightWindow,'Creating a new file ' + Filename);
 }
 {************start of 1st ***************}
  GCM1.Strplaintext[0]:='Four score and s';

  GCM1.Strplaintext[1]:='even years ago o';

  GCM1.Strplaintext[2]:='ur fathers broug';

  GCM1.Strplaintext[3]:='ht forth on this';

  AESGCMKey:=AllocMem(AES_KEY_SIZE128);
  {Now we are engag}  
  StringToBytes('4e6f772077652061726520656e676167',PByte(AESGCMKey),AES_KEY_SIZE128);
  GCM1.StrKeyHex[0]:=BytesToString(AESGCMKey,AES_BLOCK_SIZE);
   
  AESGCMTag:=AllocMem(AES_BLOCK_SIZE);
  AESGCMIV:=AllocMem(AES_BLOCK_SIZE);
  AESGCMAAD:=AllocMem(AES_BLOCK_SIZE);
  StringToBytes('000102030405060708090a0b0c0d0e0f',PByte(AESGCMIV),AES_BLOCK_SIZE);
  GCM1.StrIV:=BytesToString(AESGCMIV,AES_BLOCK_SIZE);
  StringToBytes('000102030405060708090a0b0c0d0e0f',PByte(AESGCMAAD),AES_BLOCK_SIZE);
  GCM1.StrAAD:=BytesToString(AESGCMAAD,AES_BLOCK_SIZE);
  AESGCMData:=AllocMem(AES_BLOCK_SIZE);
   
  StringToBytes('466f75722073636f726520616e642073',PByte(AESGCMData),AES_BLOCK_SIZE);
  
  ConsoleWindowWriteLn (RightWindow, 'Inputs1');
  ConsoleWindowWriteLn (RightWindow, 'Key: ' +  GCM1.StrKeyHex[0]);
  ConsoleWindowWriteLn (RightWindow, 'IV: ' +  GCM1.StrIV);
  ConsoleWindowWriteLn (RightWindow, 'AAD: ' +  GCM1.StrAAD);
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  BytesToString(AESGCMData,AES_BLOCK_SIZE));
  ConsoleWindowWriteLn (RightWindow, 'Tag: ' +  BytesToString(AESGCMTag,AES_BLOCK_SIZE));
  if AESGCMEncryptData(AESGCMKey,AES_KEY_SIZE128,AESGCMIV,AESGCMAAD,AESGCMData,AESGCMData,AES_BLOCK_SIZE,AES_BLOCK_SIZE,AES_BLOCK_SIZE,AESGCMTag) then
    begin
      GCM1.StrEnc[0]:=BytesToString(AESGCMData,AES_BLOCK_SIZE); 
      ConsoleWindowWriteLn (RightWindow, 'GCMEncrypt ok ' );
    end
  else
    begin
    ConsoleWindowWriteLn (RightWindow, 'GCMEncrypt failed');
    end;
   
  ConsoleWindowWriteLn (RightWindow, 'Key: ' +  GCM1.StrKeyHex[0]);
  ConsoleWindowWriteLn (RightWindow, 'IV: ' +  BytesToString(AESGCMIV,AES_BLOCK_SIZE));
  ConsoleWindowWriteLn (RightWindow, 'ADD: ' +  BytesToString(AESGCMAAD,AES_BLOCK_SIZE));
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  GCM1.StrEnc[0]);
  GCM1.StrKeyHex[1]:=BytesToString(AESGCMTag,AES_BLOCK_SIZE);
  {AESGCMTag becomes the key for the next block}
  ConsoleWindowWriteLn (RightWindow, 'Tag: ' + GCM1.StrKeyHex[1] );
   FreeMem(AESGCMIV);
   FreeMem(AESGCMAAD);
   FreeMem(AESGCMData);  
   FreeMem(AESGCMTag);

  AESGCMKey:=AllocMem(AES_KEY_SIZE128);
  StringToBytes(GCM1.StrKeyHex[0],PByte(AESGCMKey),AES_KEY_SIZE128);
  AESGCMTag:=AllocMem(AES_BLOCK_SIZE);
   
  AESGCMIV:=AllocMem(AES_BLOCK_SIZE);
  AESGCMAAD:=AllocMem(AES_BLOCK_SIZE);
  StringToBytes(GCM1.StrIV,PByte(AESGCMIV),AES_BLOCK_SIZE);
   
  StringToBytes(GCM1.StrAAD,PByte(AESGCMAAD),AES_BLOCK_SIZE);
   
  AESGCMData:=AllocMem(AES_BLOCK_SIZE);
  StringToBytes(GCM1.StrEnc[0],PByte(AESGCMData),AES_BLOCK_SIZE);
   
  ConsoleWindowWriteLn (RightWindow, 'Inputs Decrypt');
  ConsoleWindowWriteLn (RightWindow, 'Key: ' +  GCM1.StrKeyHex[0]);
  ConsoleWindowWriteLn (RightWindow, 'IV: ' +  GCM1.StrIV);
  ConsoleWindowWriteLn (RightWindow, 'AAD: ' +  GCM1.StrAAD);
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  GCM1.StrEnc[0]);
  
  ConsoleWindowWriteLn (RightWindow, 'Tag: ' +  BytesToString(AESGCMTag,AES_BLOCK_SIZE));
   
  
  ConsoleWindowWriteLn (RightWindow,'data to decrypt: '+GCM1.StrEnc[0]);
  if AESGCMDecryptData(AESGCMKey,AES_KEY_SIZE128,AESGCMIV,AESGCMAAD,AESGCMData,AESGCMData,AES_BLOCK_SIZE,AES_BLOCK_SIZE,AES_BLOCK_SIZE,AESGCMTag) then
    begin
      GCM1.StrDec[0]:=BytesToString(AESGCMData,AES_BLOCK_SIZE);
       
      ConsoleWindowWriteLn (RightWindow, 'GCMDecrypt ok');
    end
  else
    begin
    ConsoleWindowWriteLn (RightWindow, 'GCMDecrypt failed');
    end;
  {Since the AESGCMDecryptData is failing but the AESGCMTag is returning the  Decrypted}
  GCM1.StrDec[0]:=BytesToString(AESGCMData,AES_BLOCK_SIZE);
  ConsoleWindowWriteLn (RightWindow, 'Outputs Decrypt');
  ConsoleWindowWriteLn (RightWindow, 'Key: ' +  GCM1.StrKeyHex[0]);
  ConsoleWindowWriteLn (RightWindow, 'IV: ' +  GCM1.StrIV);
  ConsoleWindowWriteLn (RightWindow, 'ADD: ' +  GCM1.StrAAD);
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  GCM1.StrDec[0]);
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  BytesToString(AESGCMData,AES_BLOCK_SIZE));
  ConsoleWindowWriteLn (RightWindow, 'Tag: ' +  BytesToString(AESGCMTag,AES_BLOCK_SIZE));

   FreeMem(AESGCMIV);
   FreeMem(AESGCMAAD);
   FreeMem(AESGCMData);  
   FreeMem(AESGCMTag);
{************end of 1st ***************}
{************start of 2nd ***************}
  AESGCMKey:=AllocMem(AES_KEY_SIZE128);
  {798c536d4a8351dc7763d79d6434dd79}  
  StringToBytes(GCM1.StrKeyHex[1],PByte(AESGCMKey),AES_KEY_SIZE128);
  //GCM1.StrKeyHex[1]:=BytesToString(AESGCMKey,AES_BLOCK_SIZE);
   
  AESGCMTag:=AllocMem(AES_BLOCK_SIZE);
  AESGCMIV:=AllocMem(AES_BLOCK_SIZE);
  AESGCMAAD:=AllocMem(AES_BLOCK_SIZE);
  StringToBytes('000102030405060708090a0b0c0d0e0f',PByte(AESGCMIV),AES_BLOCK_SIZE);
  GCM1.StrIV:=BytesToString(AESGCMIV,AES_BLOCK_SIZE);
  StringToBytes('000102030405060708090a0b0c0d0e0f',PByte(AESGCMAAD),AES_BLOCK_SIZE);
  GCM1.StrAAD:=BytesToString(AESGCMAAD,AES_BLOCK_SIZE);
  AESGCMData:=AllocMem(AES_BLOCK_SIZE);
   
  StringToBytes('6576656e2079656172732061676f206f',PByte(AESGCMData),AES_BLOCK_SIZE);
  
  ConsoleWindowWriteLn (RightWindow, 'Inputs1');
  ConsoleWindowWriteLn (RightWindow, 'Key: ' +  GCM1.StrKeyHex[1]);
  ConsoleWindowWriteLn (RightWindow, 'IV: ' +  GCM1.StrIV);
  ConsoleWindowWriteLn (RightWindow, 'AAD: ' +  GCM1.StrAAD);
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  BytesToString(AESGCMData,AES_BLOCK_SIZE));
  ConsoleWindowWriteLn (RightWindow, 'Tag: ' +  BytesToString(AESGCMTag,AES_BLOCK_SIZE));
  if AESGCMEncryptData(AESGCMKey,AES_KEY_SIZE128,AESGCMIV,AESGCMAAD,AESGCMData,AESGCMData,AES_BLOCK_SIZE,AES_BLOCK_SIZE,AES_BLOCK_SIZE,AESGCMTag) then
    begin
      GCM1.StrEnc[1]:=BytesToString(AESGCMData,AES_BLOCK_SIZE); 
      ConsoleWindowWriteLn (RightWindow, 'GCMEncrypt ok ' );
    end
  else
    begin
    ConsoleWindowWriteLn (RightWindow, 'GCMEncrypt failed');
    end;
   
  ConsoleWindowWriteLn (RightWindow, 'Key: ' +  GCM1.StrKeyHex[1]);
  ConsoleWindowWriteLn (RightWindow, 'IV: ' +  BytesToString(AESGCMIV,AES_BLOCK_SIZE));
  ConsoleWindowWriteLn (RightWindow, 'ADD: ' +  BytesToString(AESGCMAAD,AES_BLOCK_SIZE));
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  GCM1.StrEnc[1]);
  GCM1.StrKeyHex[2]:=BytesToString(AESGCMTag,AES_BLOCK_SIZE);
  {AESGCMTag becomes the key for the next block}
  ConsoleWindowWriteLn (RightWindow, 'Tag: ' + GCM1.StrKeyHex[1] );
   FreeMem(AESGCMIV);
   FreeMem(AESGCMAAD);
   FreeMem(AESGCMData);  
   FreeMem(AESGCMTag);

  AESGCMKey:=AllocMem(AES_KEY_SIZE128);
  StringToBytes(GCM1.StrKeyHex[1],PByte(AESGCMKey),AES_KEY_SIZE128);
  AESGCMTag:=AllocMem(AES_BLOCK_SIZE);
   
  AESGCMIV:=AllocMem(AES_BLOCK_SIZE);
  AESGCMAAD:=AllocMem(AES_BLOCK_SIZE);
  StringToBytes(GCM1.StrIV,PByte(AESGCMIV),AES_BLOCK_SIZE);
   
  StringToBytes(GCM1.StrAAD,PByte(AESGCMAAD),AES_BLOCK_SIZE);
   
  AESGCMData:=AllocMem(AES_BLOCK_SIZE);
  StringToBytes(GCM1.StrEnc[1],PByte(AESGCMData),AES_BLOCK_SIZE);
   
  ConsoleWindowWriteLn (RightWindow, 'Inputs Decrypt');
  ConsoleWindowWriteLn (RightWindow, 'Key: ' +  GCM1.StrKeyHex[1]);
  ConsoleWindowWriteLn (RightWindow, 'IV: ' +  GCM1.StrIV);
  ConsoleWindowWriteLn (RightWindow, 'AAD: ' +  GCM1.StrAAD);
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  GCM1.StrEnc[1]);
  
  ConsoleWindowWriteLn (RightWindow, 'Tag: ' +  BytesToString(AESGCMTag,AES_BLOCK_SIZE));
   
  
  ConsoleWindowWriteLn (RightWindow,'data to decrypt: '+GCM1.StrEnc[1]);
  if AESGCMDecryptData(AESGCMKey,AES_KEY_SIZE128,AESGCMIV,AESGCMAAD,AESGCMData,AESGCMData,AES_BLOCK_SIZE,AES_BLOCK_SIZE,AES_BLOCK_SIZE,AESGCMTag) then
    begin
      GCM1.StrDec[1]:=BytesToString(AESGCMData,AES_BLOCK_SIZE);
       
      ConsoleWindowWriteLn (RightWindow, 'GCMDecrypt ok');
    end
  else
    begin
    ConsoleWindowWriteLn (RightWindow, 'GCMDecrypt failed');
    end;
  {Since the AESGCMDecryptData is failing but the AESGCMTag is returning the  Decrypted}
  GCM1.StrDec[1]:=BytesToString(AESGCMData,AES_BLOCK_SIZE);
  ConsoleWindowWriteLn (RightWindow, 'Outputs Decrypt');
  ConsoleWindowWriteLn (RightWindow, 'Key: ' +  GCM1.StrKeyHex[1]);
  ConsoleWindowWriteLn (RightWindow, 'IV: ' +  GCM1.StrIV);
  ConsoleWindowWriteLn (RightWindow, 'ADD: ' +  GCM1.StrAAD);
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  GCM1.StrDec[1]);
  ConsoleWindowWriteLn (RightWindow, 'Data: ' +  BytesToString(AESGCMData,AES_BLOCK_SIZE));
  ConsoleWindowWriteLn (RightWindow, 'Tag: ' +  BytesToString(AESGCMTag,AES_BLOCK_SIZE));

   FreeMem(AESGCMIV);
   FreeMem(AESGCMAAD);
   FreeMem(AESGCMData);  
   FreeMem(AESGCMTag);

 {************end of 2nd ***************}
{Let's try creating a file and writing some text to it, we'll assign our filename
   to a variable.}
  Filename:='C:\test0513.txt';

  {We should check if the file exists first before trying to create it}
  ConsoleWindowWriteLn(LeftWindow,'Checking to see if ' + Filename + ' exists');
  if FileExists(Filename) then
   begin
    {If it does exist we can delete it}
    ConsoleWindowWriteLn(LeftWindow,'Deleting the file ' + Filename);
    DeleteFile(Filename);
   end;

  {Now create the file, let's use a TFileStream class to do this. We pass both the
   filename and the mode to TFileStream. fmCreate tells it to create a new file.}
  ConsoleWindowWriteLn(LeftWindow,'Creating a new file ' + Filename);
  {TFileStream will raise an exception if creating the file fails}
  try
   FileStream:=TFileStream.Create(Filename,fmCreate);

   {We've created the file, now we need to write some content to it, we can use
    a TStringList for that but there are many other ways as well.}
   StringList:=TStringList.Create;

   {Add some text to our string list}
 StringList.Add('ASC & Hex key');
StringList.Add(CBC1.StrKeyAsc);
  StringList.Add(CBC1.StrKeyHex);
  StringList.Add('Strplaintext');
  StringList.Add(CBC1.Strplaintext[0]);
  StringList.Add(CBC1.Strplaintext[1]);
  StringList.Add(CBC1.Strplaintext[2]);
  StringList.Add(CBC1.Strplaintext[3]);
  StringList.Add(CBC1.Strplaintext[4]);
  StringList.Add(CBC1.Strplaintext[5]);
  StringList.Add(CBC1.Strplaintext[6]);
  StringList.Add(CBC1.Strplaintext[7]);
  StringList.Add('StrIV');
  StringList.Add(CBC1.StrIV[0]);
  StringList.Add(CBC1.StrIV[1]);
  StringList.Add(CBC1.StrIV[2]);
  StringList.Add(CBC1.StrIV[3]);
  StringList.Add(CBC1.StrIV[4]);
  StringList.Add(CBC1.StrIV[5]);
  StringList.Add(CBC1.StrIV[6]);
  StringList.Add(CBC1.StrIV[7]);

  StringList.Add('StrEnc');
  StringList.Add(CBC1.StrEnc[0]);
  StringList.Add(CBC1.StrEnc[1]);
  StringList.Add(CBC1.StrEnc[2]);
  StringList.Add(CBC1.StrEnc[3]);
  StringList.Add(CBC1.StrEnc[4]);
  StringList.Add(CBC1.StrEnc[5]);
  StringList.Add(CBC1.StrEnc[6]);
  StringList.Add(CBC1.StrEnc[7]);
  StringList.Add('StrDecry');
  StringList.Add(CBC1.StrDec[0]);
  StringList.Add(CBC1.StrDec[1]);
  StringList.Add(CBC1.StrDec[2]);
  StringList.Add(CBC1.StrDec[3]);
  StringList.Add(CBC1.StrDec[4]);
  StringList.Add(CBC1.StrDec[5]);
  StringList.Add(CBC1.StrDec[6]);
  StringList.Add(CBC1.StrDec[7]);
  
  StringList.Add('GCM');
  StringList.Add('Strplaintext');
  StringList.Add(GCM1.Strplaintext[0]);
  StringList.Add(GCM1.Strplaintext[1]);
  StringList.Add(GCM1.Strplaintext[2]);
  StringList.Add(GCM1.Strplaintext[3]);  
  StringList.Add('StrIV');
  StringList.Add(GCM1.StrIV);
  StringList.Add('StrAAD');
  StringList.Add(GCM1.StrAAD);
  StringList.Add('Key');
  StringList.Add(GCM1.StrKeyHex[0]);
  StringList.Add(GCM1.StrKeyHex[1]);
  StringList.Add(GCM1.StrKeyHex[2]);
  StringList.Add('Enc');
  StringList.Add(GCM1.StrEnc[0]);
  StringList.Add(GCM1.StrEnc[1]);
  StringList.Add('Dec');
  StringList.Add(GCM1.StrDec[0]);
  StringList.Add(GCM1.StrDec[1]);  
  {
  StringList.Add('Results');
  for Count:=1 to 32 do
  begin
  if (Count >1 ) then
  begin
  StringList.Add('Key '+GCM1.ActualTag[Count-1] );
  end;
  StringList.Add('Exp&Act '+GCM1.Expected[Count]+' '+GCM1.Actual[Count] );
  StringList.Add('ExpTag&ActTag '+GCM1.ExpectedTag[Count]+' '+GCM1.ActualTag[Count] );
  end;
  } 
   {Since TStringList has a SaveToStream method, we can just call that to write
    all the strings to our new file.}
   ConsoleWindowWriteLn(LeftWindow,'Saving the TStringList to the file');
   StringList.SaveToStream(FileStream);

   {With that done we can close the file and free the string list}
   ConsoleWindowWriteLn(LeftWindow,'Closing the file');
   ConsoleWindowWriteLn(LeftWindow,'');
   FileStream.Free;
   StringList.Free;

   {Did it work? Let's open the file and display it on screen to see.}
   ConsoleWindowWriteLn(LeftWindow,'Opening the file ' + Filename);
   try
    FileStream:=TFileStream.Create(Filename,fmOpenReadWrite);

    {Recreate our string list}
    StringList:=TStringList.Create;

    {And use LoadFromStream to read it}
    ConsoleWindowWriteLn(LeftWindow,'Loading the TStringList from the file');
    StringList.LoadFromStream(FileStream);

    {Iterate the strings and print them to the screen}
    ConsoleWindowWriteLn(LeftWindow,'The contents of the file are:');
    for Count:=0 to StringList.Count - 1 do
     begin
      ConsoleWindowWriteLn(LeftWindow,StringList.Strings[Count]);
     end;

    {Close the file and free the string list again}
    ConsoleWindowWriteLn(LeftWindow,'Closing the file');
    ConsoleWindowWriteLn(LeftWindow,'');
    FileStream.Free;
    StringList.Free;

    {If you remove the SD card and put in back in your computer, you should see the
     file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
     see the contents exactly as they appeared on screen.}
   except
    {TFileStream couldn't open the file}
    ConsoleWindowWriteLn(LeftWindow,'Failed to open the file ' + Filename);
   end;
  except
   {Something went wrong creating the file}
   ConsoleWindowWriteLn(LeftWindow,'Failed to create the file ' + Filename);
  end;

 ThreadHalt(0);
end.




