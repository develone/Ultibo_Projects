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
  StrData:array [1..32] of String[80];
  StrKeyHex:array [1..32] of String[80];
  
  StrIV:array [1..32] of String[40];
  StrIV64:String;
  StrAAD:array [1..32] of String[80];
  Actual:array [1..32] of String[80];
  Expected:array [1..32] of String[80];
  ActualTag:array [1..32] of String[80];
  ExpectedTag:array [1..32] of String[80];
end;
const
 AESGCMTestVectors:array[1..32,1..2] of String = (
 ('3A', '03C32E0E9D7E07A410B9BEE40A8F0D26'),
 ('26AE', '3A635BBDC1A17CA40B58CEEA78105CDC'),
 ('142FAC', '7E8922E8FA6F1E41E4339F0B52176DE4'),
 ('20C1863F', 'A1D12620C22EA7A0AA0E74667A20B8E1'),
 ('B3B796AA54', '53F0F9F03791BBD76BC99D1B5639F3C0'),
 ('FDCFF8EA82D8', 'B56076B42E3EEAC73DD42FC83B9220F9'),
 ('4695E719E67849', 'B4A1A2E29AAD713D5677CF425E65A400'),
 ('EE5BA3309D417697', '146EA95CED151F8C40DF98C1CC54930B'),
 ('13FF05ABB084FA608F', '55550AADC3461CC190CA22F29C6246CD'),
 ('008B0102208A22D3A562', '7178534BC7145754BAE525CC06E14A6B'),
 ('3536DBBB07B026E78E94C8', 'AB27183AEA2240B0166D702EEB2A7BFA'),
 ('00739D5A27AE82AC7D6A40EC', '4354578C3D241074D3C1F6496420F239'),
 ('DA41A5F458400C94B84026C052', 'DC6CB036FCAE9765A69F5B8C38B0B767'),
 ('4C99797C7EDCEA9D5425565522E2', '3FFEEC557F0D5FA73472D2A3F8E71389'),
 ('D381E7AD2E5BE2C97FB4BD958BC2EB', '6BF713D4E7DA7C4290967A1D23F97EDD'),
 ('5016C127F16A4787734AF3A3E6F6F0F7', '8CD8458531E94BC8160E2176F63F8D0B'),
 ('BDF3D0F24D9415AB5CF9B87BB45B4A8AE4', 'D81A3D56451313742ACE53D41223F6AF'),
 ('68C1FCBE22FBDB296C246F2E34D871A6902E', '7AFD64D4EB0DE7E2A842B518AC6D483F'),
 ('7D8D3C31E643611B0B557F29B437F635FE3FD0', '8501B61DBF4A4DD19B87E95055B95962'),
 ('4185EEB0B9B480F69B3EC7A162810073A36AD95A', 'B9BCA6D9CA0AC2B4B35D7BFF4DB27D25'),
 ('F991F4A481E322FEEC6FE9302D010AC4C811B23B4A', '54FA4DDA92E57509F4D48D206A03624F'),
 ('B288424FF96596B2A30A1EB9480F5EADC2F6D8551B9A', '2C998C8DFDC7663C8DE677B2F1CBCB57'),
 ('1066FE3DCB9F8AE0DC0693F7179F111E0A7A1FFE944FF4', '65402D1F8AFBDC819D6D1ADB5375AFD0'),
 ('0A8772CCDE122EFF01D7C187C77F07BDA50997B4320CD0D8', 'F55823AFC3D9FE6E749E70E82C823925'),
 ('E6E2FBB3E2238BC8CB396F463C2F488B4B4933087728D39815', 'F06DA35A9AEE65F9AD0DAD5B99AB4DF6'),
 ('569BD39CB1693CB89B88923ABE0D8CFA0B4F22A48A15E2EACD4A', '661AF51FF0E0E363406AB278BFC9176D'),
 ('199EED81C2428170EB089060FF9676596EADD2270895A0C8650903', '90AA9C634469D45E7BDD9AB955B90130'),
 ('B5200497A0654009B9F5B0D45FFDCF192F3042D6B05C6D6A8191A7EA', '71F6C4982AA50705D5FFC60512FC674C'),
 ('E39DA262C0E851B5CB5BD55A8B19D0AC0ABDC6FF3F32DF3B1896242D9E', 'B58AA05F594FC9779E185353CC52B8FB'),
 ('AF349B91BAD4BE2F2D5E4DDE28A1AA74115A9059A5EBBF9E38F341DC368B', '966B04FE43A2A9D94004E756F7DBFEFA'),
 ('8C87861DFFDE72FA64E926BF741330F64E2B30837650F309A3F979AE43BA2E', 'A5C825AE1B844D6A8D531077C881BD36'),
 ('924E178A17FA1CA0E7486F0404123B91DBF797BB9DBDE9B1D48D5C7F53165912', '10F972B6F9E0A3C1CF9CCF56543DCA79'));

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
  PCBC:=@CBC1;
  PGCM:=@GCM1;
                  {01234567890123456789012345678912}
  GCM1.StrIV64:='000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f';
  GCM1.StrKeyHex[0]:='000102030405060708090a0b0c0d0e0f';
  GCM1.StrIV[0]:=StringTrim(GCM1.StrKeyHex[0],2);
  GCM1.StrIV[1]:=StringTrim(GCM1.StrKeyHex[0],4);
  GCM1.StrIV[2]:=StringTrim(GCM1.StrKeyHex[0],6);
  GCM1.StrIV[3]:=StringTrim(GCM1.StrKeyHex[0],8);
  GCM1.StrIV[4]:=StringTrim(GCM1.StrKeyHex[0],10);
  GCM1.StrIV[5]:=StringTrim(GCM1.StrKeyHex[0],12);
  GCM1.StrIV[6]:=StringTrim(GCM1.StrKeyHex[0],14);
  GCM1.StrIV[7]:=StringTrim(GCM1.StrKeyHex[0],16);
  
  GCM1.StrIV[8]:=StringTrim(GCM1.StrKeyHex[0],18);
  GCM1.StrIV[9]:=StringTrim(GCM1.StrKeyHex[0],20);
  GCM1.StrIV[10]:=StringTrim(GCM1.StrKeyHex[0],22);
  GCM1.StrIV[11]:=StringTrim(GCM1.StrKeyHex[0],24);
  GCM1.StrIV[12]:=StringTrim(GCM1.StrKeyHex[0],26);
  GCM1.StrIV[13]:=StringTrim(GCM1.StrKeyHex[0],28);
  GCM1.StrIV[14]:=StringTrim(GCM1.StrKeyHex[0],30);
  GCM1.StrIV[15]:=StringTrim(GCM1.StrKeyHex[0],32);
  {Initization GCM1.StrIV[16] to GCM1.StrIV[19] using String GCM1.StrKeyHex[0]}
  PStrIV64:=@GCM1.StrKeyHex[0];
  GCM1.StrIV[16]:=StringTrim(PStrIV64+3,34); 
 
  GCM1.StrIV[17]:=StringTrim(PStrIV64+5,36);
 
  GCM1.StrIV[18]:=StringTrim(PStrIV64+7,38);
  
  GCM1.StrIV[19]:=StringTrim(PStrIV64+9,40); 
  {Initization GCM1.StrIV[20] to GCM1.StrIV[23] using String GCM1.StrKeyHex[0]}
  PStrIV64:=@GCM1.StrIV64;
  
  GCM1.StrIV[20]:=StringTrim(PStrIV64+3,34); 
 
  GCM1.StrIV[21]:=StringTrim(PStrIV64+5,36);
 
  GCM1.StrIV[22]:=StringTrim(PStrIV64+7,38);
  
  GCM1.StrIV[23]:=StringTrim(PStrIV64+9,40); 

  GCM1.StrData[0]:=StringTrim(GCM1.StrKeyHex[0],2);
  GCM1.StrData[1]:=StringTrim(GCM1.StrKeyHex[0],4);
  GCM1.StrData[2]:=StringTrim(GCM1.StrKeyHex[0],6);
  GCM1.StrData[3]:=StringTrim(GCM1.StrKeyHex[0],8);
  GCM1.StrData[4]:=StringTrim(GCM1.StrKeyHex[0],10);
  GCM1.StrData[5]:=StringTrim(GCM1.StrKeyHex[0],12);
  GCM1.StrData[6]:=StringTrim(GCM1.StrKeyHex[0],14);
  GCM1.StrData[7]:=StringTrim(GCM1.StrKeyHex[0],16);

  GCM1.StrData[8]:=StringTrim(GCM1.StrKeyHex[0],18);
  GCM1.StrData[9]:=StringTrim(GCM1.StrKeyHex[0],20);
  GCM1.StrData[10]:=StringTrim(GCM1.StrKeyHex[0],22);
  GCM1.StrData[11]:=StringTrim(GCM1.StrKeyHex[0],24);
  GCM1.StrData[12]:=StringTrim(GCM1.StrKeyHex[0],26);
  GCM1.StrData[13]:=StringTrim(GCM1.StrKeyHex[0],28);
  GCM1.StrData[14]:=StringTrim(GCM1.StrKeyHex[0],30);
  GCM1.StrData[15]:=StringTrim(GCM1.StrKeyHex[0],32);

  GCM1.StrAAD[0]:=StringTrim(GCM1.StrKeyHex[0],2);
  GCM1.StrAAD[1]:=StringTrim(GCM1.StrKeyHex[0],4);
  GCM1.StrAAD[2]:=StringTrim(GCM1.StrKeyHex[0],6);
  GCM1.StrAAD[3]:=StringTrim(GCM1.StrKeyHex[0],8);
  GCM1.StrAAD[4]:=StringTrim(GCM1.StrKeyHex[0],10);
  GCM1.StrAAD[5]:=StringTrim(GCM1.StrKeyHex[0],12);
  GCM1.StrAAD[6]:=StringTrim(GCM1.StrKeyHex[0],14);
  GCM1.StrAAD[7]:=StringTrim(GCM1.StrKeyHex[0],16);
  
  GCM1.StrAAD[8]:=StringTrim(GCM1.StrKeyHex[0],18);
  GCM1.StrAAD[9]:=StringTrim(GCM1.StrKeyHex[0],20);
  GCM1.StrAAD[10]:=StringTrim(GCM1.StrKeyHex[0],22);
  GCM1.StrAAD[11]:=StringTrim(GCM1.StrKeyHex[0],24);
  GCM1.StrAAD[12]:=StringTrim(GCM1.StrKeyHex[0],26);
  GCM1.StrAAD[13]:=StringTrim(GCM1.StrKeyHex[0],28);
  GCM1.StrAAD[14]:=StringTrim(GCM1.StrKeyHex[0],30);
  GCM1.StrAAD[15]:=StringTrim(GCM1.StrKeyHex[0],32);


  AESGCMKey:=AllocMem(AES_KEY_SIZE128);
  StringToBytes(GCM1.StrKeyHex[0],PByte(AESGCMKey),AES_KEY_SIZE128);
AESGCMKey:=AllocMem(AES_KEY_SIZE128);
 StringToBytes('000102030405060708090a0b0c0d0e0f',PByte(AESGCMKey),AES_KEY_SIZE128);
 AESGCMTag:=AllocMem(AES_BLOCK_SIZE);
 for Count:=1 to 32 do
  begin
   AESGCMIV:=AllocMem(Count);
   StringToBytes('000102030405060708090a0b0c0d0e0f',PByte(AESGCMIV),Count);
   AESGCMAAD:=AllocMem(Count);
   StringToBytes('000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f',PByte(AESGCMAAD),Count);
   AESGCMData:=AllocMem(Count);
   StringToBytes('000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f',PByte(AESGCMData),Count);

   if AESGCMEncryptData(AESGCMKey,AES_KEY_SIZE128,AESGCMIV,AESGCMAAD,AESGCMData,AESGCMData,Count,Count,Count,AESGCMTag) then
    begin
     GCM1.Actual[Count]:=BytesToString(AESGCMData,Count);
     GCM1.Expected[Count]:=Lowercase(AESGCMTestVectors[Count,1]); {Source: https://github.com/libtom/libtomcrypt/blob/develop/notes/gcm_tv.txt}

     GCM1.ActualTag[Count]:=BytesToString(AESGCMTag,AES_BLOCK_SIZE);
     GCM1.ExpectedTag[Count]:=Lowercase(AESGCMTestVectors[Count,2]);

     //AddItemEx(AResponse,'Expected:',Expected,3);
     //AddItemEx(AResponse,'Actual:',Actual,3);

     //AddItemEx(AResponse,'ExpectedTag:',ExpectedTag,3);
     //AddItemEx(AResponse,'ActualTag:',ActualTag,3);

     if (Uppercase(GCM1.Actual[Count]) = Uppercase(GCM1.Expected[Count])) and (Uppercase(GCM1.ActualTag[Count]) = Uppercase(GCM1.ExpectedTag[Count])) then
      begin
       //AddItemEx(AResponse,'Result:','Correct',3);
       //AddBlank(AResponse);

       System.Move(AESGCMTag^,AESGCMKey^,AES_BLOCK_SIZE);
      end
     else
      begin
       //AddItemEx(AResponse,'Result:','Incorrect',3);
       Break;
      end;
    end
   else
    begin
     //AddItemEx(AResponse,'Result:','Failed',3);
     Break;
    end;

 
   FreeMem(AESGCMIV);
   FreeMem(AESGCMAAD);
   FreeMem(AESGCMData);
 end;
{
 FreeMem(AESGCMKey);
 FreeMem(AESGCMTag);
Hash:=HashCreate(CRYPTO_HASH_ALG_HMAC_MD5,PChar('key'),3);
 if Hash <> nil then
  begin
   HashUpdate(Hash,PChar('The quick brown fox jumps over the lazy dog'),43);
   HashFinish(Hash,@MD5Digest,SizeOf(TMD5Digest));
   
   Actual:=MD5DigestToString(@MD5Digest);
   Expected:=Lowercase('80070713463e7749b90c2dc24911e275'); {Source: https://en.wikipedia.org/wiki/Hash-based_message_authentication_code}
   
   //AddItemEx(AResponse,'Expected:',Expected,3);
   //AddItemEx(AResponse,'Actual:',Actual,3);
   
   if Uppercase(Actual) = Uppercase(Expected) then
    begin
     //AddItemEx(AResponse,'Result:','Correct',3);
    end
   else
    begin
     //AddItemEx(AResponse,'Result:','Incorrect',3);
    end;
   
   HashDestroy(Hash);
  end
 else
  begin
   //AddItemEx(AResponse,'Result:','HashCreate Failed',3);
  end;  
 //AddBlank(AResponse);
 
 {Return Result}
 Result:=True;
end;
}
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
  StringList.Add(GCM1.StrIV64);
  StringList.Add(GCM1.StrKeyHex[0]);

  StringList.Add('StrIV');
  StringList.Add(GCM1.StrIV[0]);
  StringList.Add(GCM1.StrIV[1]);
  StringList.Add(GCM1.StrIV[2]);
  StringList.Add(GCM1.StrIV[3]);
  StringList.Add(GCM1.StrIV[4]);
  StringList.Add(GCM1.StrIV[5]);
  StringList.Add(GCM1.StrIV[6]);
  StringList.Add(GCM1.StrIV[7]);

  StringList.Add(GCM1.StrIV[8]); 

  StringList.Add(GCM1.StrIV[9]);
  StringList.Add(GCM1.StrIV[10]);
  StringList.Add(GCM1.StrIV[11]);
  StringList.Add(GCM1.StrIV[12]);
  StringList.Add(GCM1.StrIV[13]);
  StringList.Add(GCM1.StrIV[14]);
  StringList.Add(GCM1.StrIV[15]);
  StringList.Add('GCM1.StrIV[16] to GCM1.StrIV[19]');
  StringList.Add(GCM1.StrIV[16]);
  StringList.Add(GCM1.StrIV[17]);
  StringList.Add(GCM1.StrIV[18]);
  StringList.Add(GCM1.StrIV[19]);
  StringList.Add('GCM1.StrIV[20] to GCM1.StrIV[23]');
  StringList.Add(GCM1.StrIV[20]);
  StringList.Add(GCM1.StrIV[21]);
  StringList.Add(GCM1.StrIV[22]);
  StringList.Add(GCM1.StrIV[23]);
   
  StringList.Add('StrData');
  
  StringList.Add(GCM1.StrData[0]);
  StringList.Add(GCM1.StrData[1]);
  StringList.Add(GCM1.StrData[2]);
  StringList.Add(GCM1.StrData[3]);
  StringList.Add(GCM1.StrData[4]);
  StringList.Add(GCM1.StrData[5]);
  StringList.Add(GCM1.StrData[6]);
  StringList.Add(GCM1.StrData[7]);
  StringList.Add(GCM1.StrData[8]);
  
  StringList.Add(GCM1.StrData[9]);
  StringList.Add(GCM1.StrData[10]);
  StringList.Add(GCM1.StrData[11]);
  StringList.Add(GCM1.StrData[12]);
  StringList.Add(GCM1.StrData[13]);
  StringList.Add(GCM1.StrData[14]);
  StringList.Add(GCM1.StrData[15]);

  StringList.Add('StrAAD');
  
  StringList.Add(GCM1.StrAAD[0]);
  StringList.Add(GCM1.StrAAD[1]);
  StringList.Add(GCM1.StrAAD[2]);
  StringList.Add(GCM1.StrAAD[3]);
  StringList.Add(GCM1.StrAAD[4]);
  StringList.Add(GCM1.StrAAD[5]);
  StringList.Add(GCM1.StrAAD[6]);
  StringList.Add(GCM1.StrAAD[7]);
  StringList.Add(GCM1.StrAAD[8]);
  
  StringList.Add(GCM1.StrAAD[9]);
  StringList.Add(GCM1.StrAAD[10]);
  StringList.Add(GCM1.StrAAD[11]);
  StringList.Add(GCM1.StrAAD[12]);
  StringList.Add(GCM1.StrAAD[13]);
  StringList.Add(GCM1.StrAAD[14]);
  StringList.Add(GCM1.StrAAD[15]);

  StringList.Add('Results');

  StringList.Add(GCM1.Actual[1]);
  StringList.Add(GCM1.ActualTag[1]);
  StringList.Add(GCM1.Expected[1]);
  StringList.Add(GCM1.ExpectedTag[1]); 
  StringList.Add(GCM1.Actual[2]);
  StringList.Add(GCM1.ActualTag[2]);
  StringList.Add(GCM1.Expected[2]);
  StringList.Add(GCM1.ExpectedTag[2]); 

  StringList.Add(GCM1.Actual[3]);
  StringList.Add(GCM1.ActualTag[3]);
  StringList.Add(GCM1.Expected[3]);
  StringList.Add(GCM1.ExpectedTag[3]); 
  StringList.Add(GCM1.Actual[4]);
  StringList.Add(GCM1.ActualTag[4]);
  StringList.Add(GCM1.Expected[4]);
  StringList.Add(GCM1.ExpectedTag[4]);  
 
  StringList.Add(GCM1.Actual[5]);
  StringList.Add(GCM1.ActualTag[5]);
  StringList.Add(GCM1.Expected[5]);
  StringList.Add(GCM1.ExpectedTag[5]); 
  StringList.Add(GCM1.Actual[6]);
  StringList.Add(GCM1.ActualTag[6]);
  StringList.Add(GCM1.Expected[6]);
  StringList.Add(GCM1.ExpectedTag[6]); 

  StringList.Add(GCM1.Actual[7]);
  StringList.Add(GCM1.ActualTag[7]);
  StringList.Add(GCM1.Expected[7]);
  StringList.Add(GCM1.ExpectedTag[7]); 
  StringList.Add(GCM1.Actual[8]);
  StringList.Add(GCM1.ActualTag[8]);
  StringList.Add(GCM1.Expected[8]);
  StringList.Add(GCM1.ExpectedTag[8]);  

  StringList.Add(GCM1.Actual[9]);
  StringList.Add(GCM1.ActualTag[9]);
  StringList.Add(GCM1.Expected[9]);
  StringList.Add(GCM1.ExpectedTag[9]); 
  StringList.Add(GCM1.Actual[10]);
  StringList.Add(GCM1.ActualTag[10]);
  StringList.Add(GCM1.Expected[10]);
  StringList.Add(GCM1.ExpectedTag[10]); 

  StringList.Add(GCM1.Actual[11]);
  StringList.Add(GCM1.ActualTag[11]);
  StringList.Add(GCM1.Expected[11]);
  StringList.Add(GCM1.ExpectedTag[11]); 
  StringList.Add(GCM1.Actual[12]);
  StringList.Add(GCM1.ActualTag[12]);
  StringList.Add(GCM1.Expected[12]);
  StringList.Add(GCM1.ExpectedTag[12]);  
 
  StringList.Add(GCM1.Actual[13]);
  StringList.Add(GCM1.ActualTag[13]);
  StringList.Add(GCM1.Expected[13]);
  StringList.Add(GCM1.ExpectedTag[13]); 
  StringList.Add(GCM1.Actual[14]);
  StringList.Add(GCM1.ActualTag[14]);
  StringList.Add(GCM1.Expected[14]);
  StringList.Add(GCM1.ExpectedTag[14]); 

  StringList.Add(GCM1.Actual[15]);
  StringList.Add(GCM1.ActualTag[15]);
  StringList.Add(GCM1.Expected[15]);
  StringList.Add(GCM1.ExpectedTag[15]); 
  StringList.Add(GCM1.Actual[16]);
  StringList.Add(GCM1.ActualTag[16]);
  StringList.Add(GCM1.Expected[16]);
  StringList.Add(GCM1.ExpectedTag[16]);


  StringList.Add(GCM1.Actual[16]);
  StringList.Add(GCM1.ActualTag[16]);
  StringList.Add(GCM1.Expected[16]);
  StringList.Add(GCM1.ExpectedTag[16]); 
  StringList.Add(GCM1.Actual[17]);
  StringList.Add(GCM1.ActualTag[17]);
  StringList.Add(GCM1.Expected[17]);
  StringList.Add(GCM1.ExpectedTag[17]); 

  StringList.Add(GCM1.Actual[18]);
  StringList.Add(GCM1.ActualTag[18]);
  StringList.Add(GCM1.Expected[18]);
  StringList.Add(GCM1.ExpectedTag[19]); 
  StringList.Add(GCM1.Actual[20]);
  StringList.Add(GCM1.ActualTag[20]);
  StringList.Add(GCM1.Expected[20]);
  StringList.Add(GCM1.ExpectedTag[20]);  
 
  StringList.Add(GCM1.Actual[21]);
  StringList.Add(GCM1.ActualTag[21]);
  StringList.Add(GCM1.Expected[21]);
  StringList.Add(GCM1.ExpectedTag[21]); 
  StringList.Add(GCM1.Actual[22]);
  StringList.Add(GCM1.ActualTag[22]);
  StringList.Add(GCM1.Expected[22]);
  StringList.Add(GCM1.ExpectedTag[22]); 

  StringList.Add(GCM1.Actual[23]);
  StringList.Add(GCM1.ActualTag[23]);
  StringList.Add(GCM1.Expected[23]);
  StringList.Add(GCM1.ExpectedTag[23]); 
  StringList.Add(GCM1.Actual[24]);
  StringList.Add(GCM1.ActualTag[24]);
  StringList.Add(GCM1.Expected[24]);
  StringList.Add(GCM1.ExpectedTag[24]);  

  StringList.Add(GCM1.Actual[25]);
  StringList.Add(GCM1.ActualTag[25]);
  StringList.Add(GCM1.Expected[25]);
  StringList.Add(GCM1.ExpectedTag[25]); 
  StringList.Add(GCM1.Actual[26]);
  StringList.Add(GCM1.ActualTag[26]);
  StringList.Add(GCM1.Expected[26]);
  StringList.Add(GCM1.ExpectedTag[26]); 

  StringList.Add(GCM1.Actual[27]);
  StringList.Add(GCM1.ActualTag[27]);
  StringList.Add(GCM1.Expected[27]);
  StringList.Add(GCM1.ExpectedTag[27]); 
  StringList.Add(GCM1.Actual[28]);
  StringList.Add(GCM1.ActualTag[28]);
  StringList.Add(GCM1.Expected[28]);
  StringList.Add(GCM1.ExpectedTag[28]);  
 
  StringList.Add(GCM1.Actual[29]);
  StringList.Add(GCM1.ActualTag[29]);
  StringList.Add(GCM1.Expected[29]);
  StringList.Add(GCM1.ExpectedTag[29]); 
  StringList.Add(GCM1.Actual[30]);
  StringList.Add(GCM1.ActualTag[30]);
  StringList.Add(GCM1.Expected[30]);
  StringList.Add(GCM1.ExpectedTag[30]); 

  StringList.Add(GCM1.Actual[31]);
  StringList.Add(GCM1.ActualTag[31]);
  StringList.Add(GCM1.Expected[31]);
  StringList.Add(GCM1.ExpectedTag[31]); 
  StringList.Add(GCM1.Actual[32]);
  StringList.Add(GCM1.ActualTag[32]);
  StringList.Add(GCM1.Expected[32]);
  StringList.Add(GCM1.ExpectedTag[32]);  

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




