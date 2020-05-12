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

TWKGDATA = record
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

var
  PWKGDATA:^TWKGDATA;

  TWKGDATA1:TWKGDATA;

  //PStrKey:^String;
  LeftWindow:TWindowHandle;
  RightWindow:TWindowHandle;
  HTTPListener:THTTPListener;
  { needed to use ultibo-tftp  }
  TCP : TWinsock2TCPClient;
  IPAddress : string;
  //LP:LongWord;
 AESECBKey:PByte;

 AESECBData:PByte;
 AESECBAESKey:TAESKey;

 AESCBCKey:PByte;
 AESCBCData:PByte;
 AESCBCVector:PByte;

 Cipher:PCipherContext;

 //key:String;
 //Data:String;
 Actual:String;
 //PData:PString;
 //Datalen:LongWord;

 InKey:LongWord;
 InKeyStr:String;
 InDataStr:String;
 InIVStr:String;
 EncryptDecrypt:LongWord;
 start,stop,lftrht:LongWord;
 {These should be in Arrays Since there will many needed }
 S1,S2:String;
 //PS1,PS2,PS3,PS4:PByte;
 //NewIV:String;
 //PNewIV:PByte;
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
function encrecord(start,stop:LongWord):boolean;
var
 LP:LongWord;
begin

 for LP:=start to stop do
 begin
 InKey:=2;
 EncryptDecrypt:=1;
 InKeyStr:=TWKGDATA1.StrKeyHex;
 InIVStr:=TWKGDATA1.StrIV[LP];
 S1:=TWKGDATA1.Strplaintext[LP];
 S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
 InDataStr:=S2;

 TWKGDATA1.StrEnc[LP]:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);
 {Encrypted of previous blk bec IVector of next blk}
 TWKGDATA1.StrIV[LP+1]:=TWKGDATA1.StrEnc[LP];

 end;
 
 
 
for LP:=start to stop do
 begin
 InKey:=2;
 EncryptDecrypt:=0;
 InKeyStr:=TWKGDATA1.StrKeyHex;
 InIVStr:=TWKGDATA1.StrIV[LP];

 InDataStr:=TWKGDATA1.StrEnc[LP];

 TWKGDATA1.StrDec[LP]:= cbcencryption(InKeyStr,InDataStr,InIVStr,InKey,EncryptDecrypt);

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
 ConsoleWindowWriteLn (LeftWindow, 'Key:    ' + TWKGDATA1.StrKeyHex);
 ConsoleWindowWriteLn (LeftWindow, 'IVector:' + TWKGDATA1.StrIV[LP] );
 ConsoleWindowWriteLn (LeftWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
 ConsoleWindowWriteLn (LeftWindow, 'Data:   ' + TWKGDATA1.StrEnc[LP]);
 ConsoleWindowWriteLn (LeftWindow, 'Actual: ' + TWKGDATA1.StrDec[LP]);
 end;
 if(lftrht=1) then
 begin
 ConsoleWindowWriteLn (RightWindow, '');
 ConsoleWindowWriteLn (RightWindow, 'Key:    ' + TWKGDATA1.StrKeyHex);
 ConsoleWindowWriteLn (RightWindow, 'IVector:' + TWKGDATA1.StrIV[LP] );
 ConsoleWindowWriteLn (RightWindow, 'Mode:   ' +'Cipher Block Chaining (CBC)');
 ConsoleWindowWriteLn (RightWindow, 'Data:   ' + TWKGDATA1.StrEnc[LP]);
 ConsoleWindowWriteLn (RightWindow, 'Actual: ' + TWKGDATA1.StrDec[LP]);
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
  PWKGDATA:=@TWKGDATA1;
  TWKGDATA1.StrIV[0]:='000102030405060708090A0B0C0D0E0F';
                       {0123456789abcdef0123456789abcdef}
  TWKGDATA1.StrKeyAsc:='Now we are engaged in a great ci';
  S1:=TWKGDATA1.StrKeyAsc;
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  TWKGDATA1.StrKeyHex:=S2;
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.StrKeyAsc ' + TWKGDATA1.StrKeyAsc);
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.StrKeyHex ' + TWKGDATA1.StrKeyHex);

  TWKGDATA1.Strplaintext[0]:='Four score and s';

  TWKGDATA1.Strplaintext[1]:='even years ago o';

  TWKGDATA1.Strplaintext[2]:='ur fathers broug';

  TWKGDATA1.Strplaintext[3]:='ht forth on this';

  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[0] ' + TWKGDATA1.Strplaintext[0]);
  S1:=TWKGDATA1.Strplaintext[0];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[0] ' + S2);

  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[1] ' + TWKGDATA1.Strplaintext[1]);
  S1:=TWKGDATA1.Strplaintext[1];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[1] ' + S2);

  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[2] ' + TWKGDATA1.Strplaintext[2]);
  S1:=TWKGDATA1.Strplaintext[2];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[2] ' + S2);

  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[3] ' + TWKGDATA1.Strplaintext[3]);
  S1:=TWKGDATA1.Strplaintext[3];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[3] ' + S2);
{**************************End Init**************************}


 start:=0;
 stop:=3;
 encrecord(start,stop);
 lftrht:=0;
 disresults(start,stop,lftrht);


 {**************************Start Init4 to 7**************************}
 ConsoleWindowWriteLn (LeftWindow, '');
 TWKGDATA1.Strplaintext[4]:=' continent, a ne';

  TWKGDATA1.Strplaintext[5]:='w nation, concei';

  TWKGDATA1.Strplaintext[6]:='ved in Liberty, ';

  TWKGDATA1.Strplaintext[7]:='and dedicated to';

  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[4] ' + TWKGDATA1.Strplaintext[4]);
 S1:=TWKGDATA1.Strplaintext[4];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[4] ' + S2);
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[5] ' + TWKGDATA1.Strplaintext[5]);
  S1:=TWKGDATA1.Strplaintext[5];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[5] ' + S2);
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[6] ' + TWKGDATA1.Strplaintext[6]);
  S1:=TWKGDATA1.Strplaintext[6];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[6] ' + S2);
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[7] ' + TWKGDATA1.Strplaintext[7]);
  S1:=TWKGDATA1.Strplaintext[7];
  S2:=BytesToString(PByte(S1),Length(S1) * SizeOf(Char));
  ConsoleWindowWriteLn (LeftWindow, 'TWKGDATA1.Strplaintext[7] ' + S2);

  start:=4;
 stop:=7;
 encrecord(start,stop);
 lftrht:=1;
 disresults(start,stop,lftrht);
 ConsoleWindowWriteLn(RightWindow,'Length ' + IntToStr(length(TWKGDATA1.StrKeyHex)));
 ConsoleWindowWriteLn(RightWindow,'PWKGDATA ' + HexStr(PWKGDATA));
 ConsoleWindowWriteLn(RightWindow,'Creating a new file ' + Filename);
  {TFileStream will raise an exception if creating the file fails}
 {
 try
  FileStream:=TFileStream.Create(Filename,fmCreate);

  {We've created the file, now we need to write some content to it, we can use
   a TStringList for that but there are many other ways as well.}
  StringList:=TStringList.Create;

  {Add some text to our string list}
  StringList.Add(TWKGDATA1.StrKeyAsc);
  StringList.Add(TWKGDATA1.StrKeyHex);
  StringList.Add(TWKGDATA1.Strplaintext[0]);
  StringList.Add(TWKGDATA1.Strplaintext[1]);
  StringList.Add(TWKGDATA1.Strplaintext[2]);
  StringList.Add(TWKGDATA1.Strplaintext[3]);
  StringList.Add(TWKGDATA1.Strplaintext[4]);
  StringList.Add(TWKGDATA1.Strplaintext[5]);
  StringList.Add(TWKGDATA1.Strplaintext[6]);
  StringList.Add(TWKGDATA1.Strplaintext[7]);
  StringList.Add(TWKGDATA1.StrIV[0]);
  StringList.Add(TWKGDATA1.StrIV[1]);
  StringList.Add(TWKGDATA1.StrIV[2]);
  StringList.Add(TWKGDATA1.StrIV[3]);
  StringList.Add(TWKGDATA1.StrIV[4]);
  StringList.Add(TWKGDATA1.StrIV[5]);
  StringList.Add(TWKGDATA1.StrIV[6]);
  StringList.Add(TWKGDATA1.StrIV[7]);

  StringList.Add(TWKGDATA1.StrEnc[0]);
  StringList.Add(TWKGDATA1.StrEnc[1]);
  StringList.Add(TWKGDATA1.StrEnc[2]);
  StringList.Add(TWKGDATA1.StrEnc[3]);
  StringList.Add(TWKGDATA1.StrEnc[4]);
  StringList.Add(TWKGDATA1.StrEnc[5]);
  StringList.Add(TWKGDATA1.StrEnc[6]);
  StringList.Add(TWKGDATA1.StrEnc[7]);

  StringList.Add(TWKGDATA1.StrDec[0]);
  StringList.Add(TWKGDATA1.StrDec[1]);
  StringList.Add(TWKGDATA1.StrDec[2]);
  StringList.Add(TWKGDATA1.StrDec[3]);
  StringList.Add(TWKGDATA1.StrDec[4]);
  StringList.Add(TWKGDATA1.StrDec[5]);
  StringList.Add(TWKGDATA1.StrDec[6]);
  StringList.Add(TWKGDATA1.StrDec[7]);


  {Since TStringList has a SaveToStream method, we can just call that to write
   all the strings to our new file.}
  ConsoleWindowWriteLn(RightWindow,'Saving the TStringList to the file');
  StringList.SaveToStream(FileStream);

  {With that done we can close the file and free the string list}
  ConsoleWindowWriteLn(RightWindow,'Closing the file');
  ConsoleWindowWriteLn(RightWindow,'');
  FileStream.Free;
  StringList.Free;
   }
 {Halt this thread}
  end.

 ThreadHalt(0);
end.




