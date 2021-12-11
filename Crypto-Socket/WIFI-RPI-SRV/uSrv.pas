unit uSrv;

{$mode objfpc}{$H+}
interface

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses

  //RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,

  Classes,
  Console,
   HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  uTFTP,
  WebStatus,
  { needed for telnet }
  Shell,
  ShellFilesystem,
  ShellUpdate,
  RemoteShell,
  { needed for telnet }
  FileSystem,
  FATFS,
  MMC,
  Syscalls,
  GPIO,      {Include the GPIO unit to allow access to the functions}
  Spi,
  syncobjs,
  blcksock,
  synsock,

  crypto,
  APICrypto,
  Ultibo
  { Add additional units here };
  

   type

 TThreadManager = class;

 { TManagedThread }

 TManagedThread = class(TThread)
 public
   constructor Create(waiting : Boolean);
   function    isDone()     : Boolean;
   function    isErroneus() : Boolean;

 protected
   done_,
   erroneous_ : Boolean;
end;

  { TTCPThread }

TTCPThread = class(TManagedThread)
    private
     fSock: TTCPBlockSocket;
     fIP: string;
     FPort: integer;
     FNumber: integer;
     procedure SetSocket(aSock: TSocket);
    protected
     procedure Execute; override;
    public
     constructor Create();
     destructor Destroy; override;
     procedure ProcessingData(procSock: TSocket;SockData: string);
     {procedure ProcessEncryptDecrypt(SockData: string;ProgWindow:TWindowHandle);}
     procedure ProcessEncryptDecrypt(procSock: TSocket;SockData: string);
     Property Number: integer read Fnumber Write FNumber;
end;

 { TListenerThread }

 TListenerThread = class(TThread)
  private
    ListenerSocket: TTCPBlockSocket;
    FThreadManager: TThreadManager;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
end;

 { TThreadManager }

 TThreadManager = class(TObject)
	private
		FItemList: TThreadList;
		FAbort: Boolean;
		FThreadList: TList;
		FMaxThreadCount: Integer;
		procedure SetMaxThreadCount(Count: Integer);
	public
		constructor Create(MaxThreads: integer);
		destructor Destroy; override;
		procedure AddItem(Item: TTCPThread);
		function GetSuspendThread(aSock: TSocket): TTCPThread;
                procedure clearFinishedThreads;
		function GetActiveThreadCount: Integer;
		property MaxThreadCount: Integer read FMaxThreadCount write SetMaxThreadCount;
	end;

  {procedure TThreadManager.SetMaxThreadCount(Count: Integer);
  function TThreadManager.GetActiveThreadCount: Integer;
  procedure TThreadManager.AddItem(Item: TTCPThread);
  procedure TThreadManager.clearFinishedThreads;
  function  TManagedThread.isErroneus() : Boolean;
  constructor TManagedThread.Create(waiting : Boolean);
  destructor TListenerThread.Destroy;
  constructor TListenerThread.Create;
  procedure TTCPThread.SetSocket(aSock: TSocket);
  procedure TTCPThread.Execute;
  constructor TTCPThread.Create();
  destructor TTCPThread.Destroy;
  procedure TTCPThread.ProcessEncryptDecrypt(procSock: TSocket;SockData: string);}

  
  implementation



{ TThreadManager }


procedure TThreadManager.SetMaxThreadCount(Count: Integer);
begin
  FMaxThreadCount := Count;
end;

constructor TThreadManager.Create(MaxThreads: integer);
begin
  inherited Create;
	FItemList := TThreadList.Create;
	FThreadList := TList.Create;
        FMaxThreadCount := MaxThreads;
end;

destructor TThreadManager.Destroy;
var
	i: Integer;
begin
    FThreadList.Pack;
	for i := FThreadList.Count - 1 downto 0 do begin
    	TTCPThread(FThreadList[i]).Free;
	end;
    FThreadList.Capacity := FThreadList.Count;
	FThreadList.Free;
    FItemList.Clear;
	FItemList.Free;
	inherited;
end;

procedure TThreadManager.AddItem(Item: TTCPThread);
begin
  FItemList.Add(Pointer(Item));
end;

function TThreadManager.GetSuspendThread(aSock: TSocket): TTCPThread;
var
	i: Integer;
	TCPThread: TTCPThread;
begin
	Result := nil;
	if GetActiveThreadCount >= FMaxThreadCount then Exit;
	for i := 0 to FThreadList.Count - 1 do begin
		if TTCPThread(FThreadList[i]).Suspended then
                 begin
			TCPThread := TTCPThread(FThreadList[i]);
                        TCPThread.SetSocket(aSock);
                        TCPThread.Resume;
			Break;
		end;
	end;
	if (Result = nil) and (FMaxThreadCount > FThreadList.Count) then begin
		TCPThread := TTCPThread.Create;
		TCPThread.FreeOnTerminate := False;
                TCPThread.SetSocket(aSock);
		TCPThread.Number := FThreadList.Count;
		FThreadList.Add(TCPThread);
		Result := TCPThread;
	end;
end;

procedure TThreadManager.clearFinishedThreads;
var
	i: Integer;
begin
	for i := 0 to FThreadList.Count - 1 do
         begin
           if (TTCPThread(FThreadList[i]) <> nil) and TTCPThread(FThreadList[i]).isDone() then
               begin
                 TTCPThread(FThreadList[i]).WaitFor;
                 TTCPThread(FThreadList[i]).Free;
         end;

end;
end;

function TThreadManager.GetActiveThreadCount: Integer;
var
	i: Integer;
begin
	Result := 0;
	for i := 0 to FThreadList.Count - 1 do begin
		if not TTCPThread(FThreadList[i]).Suspended then
			Inc(Result);
	end;
end;

{ TManagedThread }

constructor TManagedThread.Create(waiting : Boolean);
begin
 inherited Create(waiting);
 done_ := false;
 erroneous_ := false;
end;

function  TManagedThread.isDone()     : Boolean;
begin
 Result := done_;
end;


function  TManagedThread.isErroneus() : Boolean;
begin
 Result := erroneous_;
end;

{ TListenerThread }

procedure TListenerThread.Execute;


var

ClientSock : TSocket;
ClientThread : TTCPThread;
WindowHandle : TWindowHandle;
lclfn : String;
nr : integer;
datab : Byte;
bp : ^Byte;
rdp : ^Byte;
CmdArray : array [0..11] of Byte;
RDArray : array [0..11] of Byte;
ff : Boolean;
{0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb7 0xb0 0xb0 0xb0 0xb7 0x8a < A1009W70007
0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb3 0xb0 0xb0 0xb0 0xb3 0x8a < A1009W30003
0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb1 0xb0 0xb0 0xb0 0xb1 0x8a < A1009W10001

0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb7 0xb0 0xb0 0xb0 0xb0 0x8a < A1009W70000
0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb3 0xb0 0xb0 0xb0 0xb0 0x8a < A1009W30000
0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb1 0xb0 0xb0 0xb0 0xb0 0x8a < A1009W10000}


 begin

  bp := @CmdArray;
  rdp := @RDArray;
  {0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb7 0xb0 0xb0 0xb0 0xb7 0x8a < A1009W70007}
  CmdArray[0] := 193; // 0xC1 0x41
  CmdArray[1] := 177; // 0xB1 0x31
  CmdArray[2] := 176; // 0xB0 0x30
  CmdArray[3] := 176; // 0xB0 0x30
  CmdArray[4] := 185; // 0xB9 0x39
  CmdArray[5] := 215; // 0xD7 0x57
  CmdArray[6] := 183; // 0xB7 0x37 0xB1 0xB3
  CmdArray[7] := 176; // 0xB0 0x30
  CmdArray[8] := 176; // 0xB0 0x30
  CmdArray[9] := 176; // 0xB0 0x30
  CmdArray[10] := 183; // 0xB7 0x37 0xB1 0xB3
  CmdArray[11] := 138; // 0x8A 0x0a

  //lclfn:='speechpp.bin';
  //lclfn:='catzip.bin';
  WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);



  nr := 11;



  //if (ff )  then ConsoleWindowWriteLn(WindowHandle,'FPGA was program');
   with ListenerSocket do
     begin
       CreateSocket;
        if LastError = 0 then
           begin
           ConsoleWindowWriteLn(WindowHandle,'Socket successfully initialized');

           end
          else
           ConsoleWindowWriteLn(WindowHandle,'An error occurred while initializing the socket: '+GetErrorDescEx);
   Family := SF_IP4;
   setLinger(true,10000);
   bind('0.0.0.0', '5050');
    if LastError = 0 then
      begin
      ConsoleWindowWriteLn(WindowHandle,'Bind on 5050');
      end
     else
      ConsoleWindowWriteLn(WindowHandle,'Bind error: '+GetErrorDescEx);
      listen;
      repeat
        if CanRead(100) then
         begin
           ClientSock := Accept;
            if LastError = 0
             then
              begin
              //TTCPThread.Create()
             ClientThread:=FThreadManager.GetSuspendThread(ClientSock);
              ConsoleWindowWriteLn(WindowHandle,'We have '+ IntToStr(FThreadManager.GetActiveThreadCount)+#32+'client threads!');
              end
             else
              ConsoleWindowWriteLn(WindowHandle,'TCP thread creation error: '+GetErrorDescEx);
         end;
        FThreadManager.clearFinishedThreads;
      sleep(10);
     until false;
    end;
end;

constructor TListenerThread.Create;
begin
FreeOnTerminate := True;
ListenerSocket := TTCPBlockSocket.Create;
FThreadManager:=TThreadManager.Create(20000);
if ListenerSocket.LastError = 0
  then
     WriteLn('Listener has been created')
  else
      WriteLn('Listener creation error: '+ListenerSocket.GetErrorDescEx);
inherited Create(False);
end;

destructor TListenerThread.Destroy;
begin
 ListenerSocket.Free;
   if
     ListenerSocket.LastError = 0
       then
           WriteLn('Listener has been deleted')
          else
            WriteLn('Listener deleting error: '+ListenerSocket.GetErrorDescEx);
  inherited;
end;

{ TTCPThread }

procedure TTCPThread.SetSocket(aSock: TSocket);
begin
   fSock.Socket := aSock;
   fSock.GetSins;
end;

procedure TTCPThread.Execute;
var
  s: ansistring;
begin
  fIp:=fSock.GetRemoteSinIP;
  fPort:=fSock.GetRemoteSinPort;
  WriteLn(format('Accepted connection from %s:%d',[fIp,fPort]));
  while not isDone  do
   begin
    if fSock.WaitingData > 0 then
     begin
      s:=fSock.RecvPacket(2000);
      if fSock.LastError <> 0 then
       WriteLn(fSock.GetErrorDescEx);
       ProcessingData(fSock.Socket,S);
      end;
    sleep(10);
   end;
end;

constructor TTCPThread.Create();
begin
 FreeOnTerminate := True;
 fSock := TTCPBlockSocket.Create;
 inherited Create(false);
end;

destructor TTCPThread.Destroy;
begin
  WriteLn(format('Disconnect from %s:%d',[fIp,fPort]));
  fSock.Free;
  inherited;
end;

{procedure TTCPThread.ProcessEncryptDecrypt(SockData : string;ProgWindow:TWindowHandle);}
procedure TTCPThread.ProcessEncryptDecrypt(procSock: TSocket;SockData: string);
{112345678901234567890123456789012:My Secret IV:My Extra Secret AAD:The quick brown The quick brown The quick brown The quick brown The quick brown The quick brown}
{212345678901234567890123456789012:My Secret IV:My Extra Secret AAD:}
type
GCM = record
  SockData:AnsiString;
  EncryptionTagToDecrypt:AnsiString;
  {EncryptionTag1 during teststr 1 or teststr 3 encrypt}
  EncryptionTag1:AnsiString;
  {EncryptionTag2 during teststr 1 or teststr 3 decrypt}
  EncryptionTag2:AnsiString;
  EncryptionTag3:AnsiString;
  PlainStr:AnsiString;
  CryptStr1:AnsiString;
  BinCryptStr1:AnsiString;
  cryptstr: AnsiString;
  tagstr: AnsiString;
  teststr: AnsiString;
  {Must be 16, 24 or 32 bytes}
  MyKey: AnsiString ; 
  MyIV: AnsiString;
  MyAAD: AnsiString;
  MyData: AnsiString;
end;
var
  {GCM1 during teststr 1
  GCM2 during teststr 2
  GCM3 during teststr 3}
  GCM1,GCM2,GCM3:GCM;
 
  mybyte:Byte;
  lendata:Longword;
  EncryptionTagToDecrypt:AnsiString;
  EncryptionTag:AnsiString;
  PlainStr:AnsiString;
  CryptStr1:AnsiString;
  BinCryptStr1:AnsiString;  
  CC:LongWord;
  off:LongWord;
  Count:Integer;
  Filename:String;
  SearchRec:TSearchRec;
  StringList:TStringList;
  FileStream:TFileStream;
  cryptstr: AnsiString;
  tagstr: AnsiString; 
  newstr: AnsiString;
  teststr: AnsiString;
  DatainLen:LongWord;
  comindex:Integer;
  MyKey: AnsiString = '1234567890123456'; {Must be 16, 24 or 32 bytes}
  MyIV: AnsiString = 'My Secret IV';
  MyAAD: AnsiString = 'My Extra Secret AAD';
  MyData: AnsiString = 'The quick brown fox jumps over the lazy dog.The quick brown fox jumps over the lazy dog.';
  MyResult: AnsiString;
  EOL: AnsiString;
  Key: PByte;
  IV: PByte;
  AAD: PByte;
  Plain: PByte;
  Crypt: PByte;
  Tag: PByte;
  testptr:PByte;
begin
  EOL:='  ';
  EOL[1]:=char(13);
  EOL[2]:=char(10); 
  DatainLen:=Length(SockData);
  newstr:=RightStr(SockData,DatainLen-1);
  teststr:=LeftStr(SockData,1);
  //WriteLn(teststr);
  //WriteLn(IntToStr(DatainLen));
  //WriteLn(newstr);
//chgs planned for Srv.lpr which is the same as Srv.lpr.tmp3
 

{**************************encryption**************************}  
  if (teststr='1') then
begin  
  WriteLn('1'); 
GCM1.SockData:=SockData;      
WriteLn('Key '+MyKey);
WriteLn('IV '+MyIV);
WriteLn('AAD '+MyAAD);
WriteLn('Data '+MyData);
comindex:=LastDelimiter('\.:',SockData);


WriteLn('index where data ' +IntToStr(comindex));
MyData:=RightStr(SockData,DatainLen-comindex);
 
DatainLen:=Length(MyData);
WriteLn('MyData '+MyData+ 'Length with EOL '+IntToStr(DatainLen));
{Since MyData has EOL.  The above WriteLn writes the DatainLen on a new line}
SockData:=LeftStr(SockData,comindex-1);
 
DatainLen:=Length(SockData);
WriteLn('SockData '+SockData + ' '+IntToStr(DatainLen));

comindex:=LastDelimiter('\.:',SockData);
WriteLn('index where AAD ' +IntToStr(comindex));
MyAAD:=RightStr(SockData,DatainLen-comindex);
WriteLn('MyAAD '+MyAAD);
SockData:=LeftStr(SockData,comindex-1);
 
DatainLen:=Length(SockData);
WriteLn('SockData '+SockData+ ' '+IntToStr(DatainLen));
comindex:=LastDelimiter('\.:',SockData);
WriteLn('index where IV ' +IntToStr(comindex));
MyIV:=RightStr(SockData,DatainLen-comindex);
WriteLn('MyIV '+MyIV);
SockData:=LeftStr(SockData,comindex-1);

DatainLen:=Length(SockData);
WriteLn('SockData '+SockData+ ' '+IntToStr(DatainLen));
MyKey:=RightStr(SockData,DatainLen-1);
WriteLn('MyKey '+MyKey);

lendata:=Length(MyData) - 2;
 
WriteLn('without no EOL lendata ',IntToStr(lendata)); 
  {Allocate buffers}
  Key := AllocMem(Length(MyKey));
  IV := AllocMem(Length(MyIV));
  AAD := AllocMem(Length(MyAAD));
  Plain := AllocMem(lendata);
  Crypt := AllocMem(lendata);
  Tag := AllocMem(AES_BLOCK_SIZE);
  testptr := AllocMem(lendata);
  {Copy the values}
  Move(MyKey[1], Key^, Length(MyKey));
  Move(MyIV[1], IV^, Length(MyIV));
  Move(MyAAD[1], AAD^, Length(MyAAD));
  Move(MyData[1], Plain^, lendata);
  WriteLn('Key '+MyKey+' '+IntToStr(Length(MyKey)));
  WriteLn('IV '+MyIV+' '+IntToStr(Length(MyIV)));
  WriteLn('AAD '+MyAAD+' '+IntToStr(Length(MyAAD)));
  
  GCM1.MyKey:=MyKey;
  GCM1.MyIV:=MyIV;
  GCM1.MyAAD:=MyAAD;
  
  {Clear the crypt buffer}
  FillChar(Crypt^, lendata, 0);
  {Encrypt the data}
  if AESGCMEncryptData(Key, Length(MyKey), IV, AAD, Plain, Crypt, Length(MyIV), Length(MyAAD), lendata, Tag) then
  begin
    WriteLn('AES GCM Encrypt Success');
    BinCryptStr1:=BytesToString(Crypt,lendata);
    fSock.SendString(BinCryptStr1);
    fSock.SendString(EOL); 
    WriteLn('Bytes from Crypt ');
    {Clear the plain buffer}
    FillChar(Plain^, lendata, 0);
    for CC:= 0 to lendata - 1 do
    begin
    mybyte:=Crypt^;
    Inc(Crypt);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(Crypt,lendata);
    WriteLn(' ');
    EncryptionTag:=BytesToString(Tag,AES_BLOCK_SIZE);
    GCM1.EncryptionTag1:=EncryptionTag;
    WriteLn('EncryptionTag ', EncryptionTag);
    {Decrypt the Data}
    if AESGCMDecryptData(Key, Length(MyKey), IV, AAD, Crypt, Plain, Length(MyIV), Length(MyAAD), lendata, Tag) then
    begin
      WriteLn('AES GCM Decrypt Success');
    WriteLn('Tag '+Hexstr(@Tag));  
    for CC:= 0 to AES_BLOCK_SIZE - 1 do
    begin
    mybyte:=Tag^;
    Inc(Tag);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(Tag,AES_BLOCK_SIZE);
    WriteLn(' ');
WriteLn('Tag '+Hexstr(@Tag));
      {Copy the result}
      SetString(PlainStr, PAnsiChar(Plain), lendata);
      fSock.SendString(PlainStr);
      fSock.SendString(EOL);
      WriteLn('Ascii PlainStr is ' + PlainStr);
      GCM1.PlainStr:=PlainStr;
      //SetString(MyResult, PAnsiChar(Crypt), lendata);
      EncryptionTag:=BytesToString(Tag,AES_BLOCK_SIZE);
      GCM1.EncryptionTag2:=EncryptionTag;
      fSock.SendString(EncryptionTag);
      fSock.SendString(EOL);
    WriteLn('EncryptionTag ', EncryptionTag);
      for CC:= 0 to lendata - 1 do
    begin
    mybyte:=Plain^;
    Inc(Plain);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(Plain,lendata);
    WriteLn(' ');
    
      {Convert Crypt from Bytes to a String}
      MyResult:=BytesToString(Crypt,lendata);
      WriteLn('BytesToString Crypt '+ MyResult );
      
    end
    else
    begin
      WriteLn('AES GCM Decrypt Failure');
    end;
  end
  else
  begin
    WriteLn('AES GCM Encrypt Failure');
  end;

FreeMem(Key);
FreeMem(IV);
FreeMem(AAD);
FreeMem(Plain);
FreeMem(Crypt);
FreeMem(Tag);
FreeMem(testptr);
end;
{**************************End encryption**************************}
{**************************decryption**************************}
if (teststr='2') then
begin
  WriteLn('2');
  GCM2.SockData:=SockData;
WriteLn('Key '+MyKey);
WriteLn('IV '+MyIV);
WriteLn('AAD '+MyAAD);
WriteLn('Data '+MyData);

comindex:=LastDelimiter('\.:',SockData);
WriteLn('index where tag ' +IntToStr(comindex));
tagstr:=RightStr(SockData,DatainLen-comindex);
GCM2.tagstr:=tagstr;
WriteLn('tagstr '+tagstr+' '+IntToStr(Length(tagstr)));
SockData:=LeftStr(SockData,comindex-1);
DatainLen:=Length(SockData);

comindex:=LastDelimiter('\.:',SockData);
WriteLn('index where data '+IntToStr(comindex));
MyData:=RightStr(SockData,DatainLen-comindex);
WriteLn('MyData '+MyData+' '+IntToStr(Length(MyData)));
SockData:=LeftStr(SockData,comindex-1);
DatainLen:=Length(SockData);
 
comindex:=LastDelimiter('\.:',SockData);
WriteLn('index where AAD '+IntToStr(comindex));
MyAAD:=RightStr(SockData,DatainLen-comindex);
WriteLn('MyAAD '+MyAAD+' '+IntToStr(Length(MyAAD)));
SockData:=LeftStr(SockData,comindex-1);
DatainLen:=Length(SockData);
 
comindex:=LastDelimiter('\.:',SockData);
WriteLn('index where IV '+IntToStr(comindex));
MyIV:=RightStr(SockData,DatainLen-comindex);
WriteLn('MyIV '+MyIV+' '+IntToStr(Length(MyIV)));
SockData:=LeftStr(SockData,comindex-1);
DatainLen:=Length(SockData);
 
WriteLn('SockData '+SockData+' '+IntToStr(DatainLen));
MYKey:=RightStr(SockData,DatainLen-1);
WriteLn('MYKey '+MYKey+' '+IntToStr(Length(MYKey)));
 
  
  GCM2.MyKey:=MyKey;
  GCM2.MyIV:=MyIV;
  GCM2.MyAAD:=MyAAD;
  


cryptstr:=MyData;

{WriteLn('cryptstr '+cryptstr + ' ' + IntToStr(Length(cryptstr)));}
WriteLn('cryptstr '+cryptstr);
DatainLen:=Length(cryptstr);
 
lendata:=(Length(MyData) ) div 2;
WriteLn('without no EOL lendata ',IntToStr(lendata));  
  testptr := AllocMem(lendata);
  Crypt := AllocMem(lendata);
StringToBytes(cryptstr,PByte(testptr),lendata);
MyResult:=BytesToString(testptr,lendata);
WriteLn('MyResult '+MyResult);

StringToBytes(cryptstr,PByte(Crypt),lendata);
{
WriteLn('testptr '+Hexstr(@testptr)+ 'Crypt '+Hexstr(@Crypt));
    for CC:= 0 to lendata - 1 do
    begin
    mybyte:=testptr^;
    Inc(testptr);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(testptr,lendata);
    WriteLn(' ');
    for CC:= 0 to lendata - 1 do
    begin
    mybyte:=Crypt^;
    Inc(Crypt);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(Crypt,lendata);
    WriteLn(' ');
WriteLn('testptr '+Hexstr(@testptr)+ 'Crypt '+Hexstr(@Crypt));
} 
  Key := AllocMem(Length(MyKey));
  IV := AllocMem(Length(MyIV));
  AAD := AllocMem(Length(MyAAD));
  Plain := AllocMem(lendata);
   
  Tag := AllocMem(AES_BLOCK_SIZE);
  StringToBytes(tagstr,PByte(Tag),AES_BLOCK_SIZE);
  {Copy the values}
  Move(MyKey[1], Key^, Length(MyKey));
  Move(MyIV[1], IV^, Length(MyIV));
  Move(MyAAD[1], AAD^, Length(MyAAD));
  

{
WriteLn('IV '+Hexstr(@IV));  

    for CC:= 0 to (Length(MyIV) -1) do
    begin
    mybyte:=IV^;
    Inc(IV);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(IV,(Length(MyIV) -1));
    WriteLn(' ');
WriteLn('IV '+Hexstr(@IV));  
WriteLn('AAD '+Hexstr(@AAD));  

    for CC:= 0 to (Length(MyAAD) -1) do
    begin
    mybyte:=AAD^;
    Inc(AAD);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(AAD,(Length(MyAAD) -1));
    WriteLn(' ');
WriteLn('AAD '+Hexstr(@AAD)); 
 
WriteLn('Key '+Hexstr(@Key));  

    for CC:= 0 to (Length(MyKey) -1) do
    begin
    mybyte:=Key^;
    Inc(Key);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(Key,(Length(MyKey) -1));
    WriteLn(' ');
WriteLn('Key '+Hexstr(@Key)); 
 
  {Clear the plain buffer}
  FillChar(Plain^, lendata, 0);
WriteLn('Plain '+Hexstr(@Plain));  
    for CC:= 0 to lendata - 1 do
    begin
    mybyte:=Plain^;
    Inc(Plain);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(Plain,lendata);
    WriteLn(' ');
WriteLn('Plain '+Hexstr(@Plain)); 
}
  
{This did not help 
StringToBytes('d495df0bc8a0f10d5aba11764e898070',PByte(Tag),AES_BLOCK_SIZE);

going to test adding the encryption here}
  WriteLn('Key '+MyKey+' '+IntToStr(Length(MyKey)));
  WriteLn('IV '+MyIV+' '+IntToStr(Length(MyIV)));
  WriteLn('AAD '+MyAAD+' '+IntToStr(Length(MyAAD)));
  GCM2.MyKey:=MyKey;
  GCM2.MyIV:=MyIV;
  GCM2.MyAAD:=MyAAD;
 FillChar(Plain^, lendata, 0);
StringToBytes(cryptstr,PByte(Crypt),lendata);
StringToBytes(tagstr,PByte(Tag),AES_BLOCK_SIZE); 
//StringToBytes('d495df0bc8a0f10d5aba11764e898070',PByte(Tag),AES_BLOCK_SIZE);
EncryptionTagToDecrypt:=BytesToString(PByte(Tag),AES_BLOCK_SIZE);
GCM2.EncryptionTagToDecrypt:=EncryptionTagToDecrypt;
WriteLn('Tag '+Hexstr(@Tag));  
    for CC:= 0 to AES_BLOCK_SIZE - 1 do
    begin
    mybyte:=Tag^;
    Inc(Tag);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(Tag,AES_BLOCK_SIZE);
    WriteLn(' ');
WriteLn('Tag '+Hexstr(@Tag));
{Decrypt the Data}
    if AESGCMDecryptData(Key, Length(MyKey), IV, AAD, Crypt, Plain, Length(MyIV), Length(MyAAD), lendata, Tag) then
    begin
      WriteLn('AES GCM Decrypt Success');

      {Copy the result}
      SetString(PlainStr, PAnsiChar(Plain), lendata);
      fSock.SendString(PlainStr);
      fSock.SendString(EOL);
      WriteLn('Ascii PlainStr is ' + PlainStr);
      GCM2.PlainStr:=PlainStr;
      //SetString(MyResult, PAnsiChar(Crypt), lendata);
      EncryptionTag:=BytesToString(Tag,AES_BLOCK_SIZE);
      GCM2.EncryptionTag2:=EncryptionTag;
      {
      for CC:= 0 to lendata - 1 do
    begin
    mybyte:=Plain^;
    Inc(Plain);
    Write(IntToStr(mybyte)+' ');
    end;
    Dec(Plain,lendata);
    WriteLn(' ');
    }
      {Convert Crypt from Bytes to a String}
      CryptStr:=BytesToString(Crypt,lendata);
      WriteLn('BytesToString Crypt '+ CryptStr );
      
    end
    else
    begin
      WriteLn('AES GCM Decrypt Failure');
    end;
 
FreeMem(Key);
FreeMem(IV);
FreeMem(AAD);
FreeMem(Plain);
FreeMem(Crypt);
FreeMem(Tag);
FreeMem(testptr);

end;
{**************************end decryption**************************}
{**************************3**************************}
if (teststr='3') then
begin


end;
{**************************end 3**************************}
if (teststr='4') then
    WriteLn('4');


end;
procedure TTCPThread.ProcessingData(procSock: TSocket; SockData: string);
begin
  if SockData <> '' then

   begin
   WriteLn(SockData+#32+'we get it from '+IntToStr(number)+' thread');
   //ProcessFpga(Data,TTCPThread.WindowHandle);
   //WriteLn('Calling ProcessFpga');
   ProcessEncryptDecrypt(procSock,SockData);
   //WriteLn(SockData);
   end;
end;


 
end.

