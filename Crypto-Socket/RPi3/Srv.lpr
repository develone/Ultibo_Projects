program Srv;

{$mode objfpc}{$H+}

{ Raspberry Pi 2 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses

  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  BCM2837,
  BCM2710,
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
  uFPGA,
  crypto,
  Ultibo
  { Add additional units here };
{var
  WindowHandle:TWindowHandle;
  Fn:String;
  flg1:Boolean;}

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
     procedure ProcessingData(procSock: TSocket;Data: string);
     {procedure ProcessEncryptDecrypt(Data: string;ProgWindow:TWindowHandle);}
     procedure ProcessEncryptDecrypt(Data: string);
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

{procedure TTCPThread.ProcessEncryptDecrypt(Data : string;ProgWindow:TWindowHandle);}
procedure TTCPThread.ProcessEncryptDecrypt(Data: string);
{412345678901234567890123456789012:My Secret IV:My Extra Secret AAD:The quick brown The quick brown The quick brown The quick brown The quick brown The quick brown}
var
  CC:LongWord;
  Count:Integer;
  Filename:String;
  SearchRec:TSearchRec;
  StringList:TStringList;
  FileStream:TFileStream;
   
  newstr: AnsiString;
  teststr: AnsiString;
  DatainLen:LongWord;
  comindex:Integer;
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
begin
   
  DatainLen:=Length(Data);
  newstr:=RightStr(Data,DatainLen-1);
  teststr:=LeftStr(Data,1);
  WriteLn(teststr);
  WriteLn(IntToStr(DatainLen));
  WriteLn(newstr);
  if (teststr='4') then
    begin
WriteLn('Key '+MyKey);
WriteLn('IV '+MyIV);
WriteLn('AAD '+MyAAD);
WriteLn('Data '+MyData);
comindex:=LastDelimiter('\.:',Data);
WriteLn(IntToStr(comindex));
MyData:=RightStr(Data,DatainLen-comindex);
WriteLn('MyData '+MyData);
Data:=LeftStr(Data,comindex-1);
WriteLn(Data);
DatainLen:=Length(Data);
WriteLn(IntToStr(DatainLen));

comindex:=LastDelimiter('\.:',Data);
WriteLn(IntToStr(comindex));
MyAAD:=RightStr(Data,DatainLen-comindex);
WriteLn('MyAAD '+MyAAD);
Data:=LeftStr(Data,comindex-1);
WriteLn(Data);
DatainLen:=Length(Data);

comindex:=LastDelimiter('\.:',Data);
WriteLn(IntToStr(comindex));
MyIV:=RightStr(Data,DatainLen-comindex);
WriteLn('MyIV '+MyIV);
Data:=LeftStr(Data,comindex-1);
WriteLn(Data);
DatainLen:=Length(Data);
MyKey:=RightStr(Data,DatainLen-1);
WriteLn('MyKey '+MyKey);

 
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
    WriteLn('AES GCM Encrypt Success');

    {Clear the plain buffer}
    FillChar(Plain^, Length(MyData), 0);

    {Decrypt the Data}
    if AESGCMDecryptData(Key, Length(MyKey), IV, AAD, Crypt, Plain, Length(MyIV), Length(MyAAD), Length(MyData), Tag) then
    begin
      WriteLn('AES GCM Decrypt Success');

      {Copy the result}
      SetString(MyResult, PAnsiChar(Plain), Length(MyData));

      WriteLn('MyResult is ' + MyResult);
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
 
end;

{First let's list the contents of the SD card. We can guess that it will be C:\
  drive because we didn't include the USB host driver.}
 WriteLn('Contents of drive C:\');

 {To list the contents we need to use FindFirst/FindNext, start with FindFirst}
 if FindFirst('C:\*.*',faAnyFile,SearchRec) = 0 then
  begin
   {If FindFirst succeeds it will return 0 and we can proceed with the search}
   repeat
    {Print the file found to the screen}
    //WriteLn('Filename is ' + SearchRec.Name + ' - Size is ' + IntToStr(SearchRec.Size) + ' - Time is ' + DateTimeToStr(FileDateToDateTime(SearchRec.Time)));

   {We keep calling FindNext until there are no more files to find}
   until FindNext(SearchRec) <> 0;
  end;

{After any call to FindFirst, you must call FindClose or else memory will be leaked}
 FindClose(SearchRec);
 WriteLn('');
{Let's try creating a file and writing some text to it, we'll assign our filename
   to a variable.}
  Filename:='C:\test0527a.txt';

  {We should check if the file exists first before trying to create it}
  WriteLn('Checking to see if ' + Filename + ' exists');
  if FileExists(Filename) then
   begin
    {If it does exist we can delete it}
    WriteLn('Deleting the file ' + Filename);
    DeleteFile(PChar(Filename));
   end;

  {Now create the file, let's use a TFileStream class to do this. We pass both the
   filename and the mode to TFileStream. fmCreate tells it to create a new file.}
  WriteLn('Creating a new file ' + Filename);
  {TFileStream will raise an exception if creating the file fails}
  try
   FileStream:=TFileStream.Create(Filename,fmCreate);

   {We've created the file, now we need to write some content to it, we can use
    a TStringList for that but there are many other ways as well.}
   StringList:=TStringList.Create;

   {Add some text to our string list}
 StringList.Add(MyKey);
 StringList.Add(MyIV);
 StringList.Add(MyAAD);
 StringList.Add(MyData);
 StringList.Add(MyResult);
 //SetString(MyResult, PAnsiChar(Crypt), Length(MyData));
 MyResult:='';
 for CC:=0 to Length(MyData) - 1 do
  begin
   MyResult:=MyResult + HexStr(Crypt[CC],2);
  end;
 MyResult:=Lowercase(MyResult); 
 StringList.Add(MyResult);
  {Since TStringList has a SaveToStream method, we can just call that to write
   all the strings to our new file.}
  WriteLn('Saving the TStringList to the file');
  StringList.SaveToStream(FileStream);
 {Iterate the strings and print them to the screen}
    WriteLn('The contents of the file are:');
    for Count:=0 to StringList.Count - 1 do
     begin
      WriteLn(StringList.Strings[Count]);
     end;
  {With that done we can close the file and free the string list}
   WriteLn('Closing the file');
   WriteLn('');
   FileStream.Free;
   StringList.Free;

   {Did it work? Let's open the file and display it on screen to see.}
   WriteLn('Opening the file ' + Filename);
   try
    FileStream:=TFileStream.Create(Filename,fmOpenReadWrite);

    {Recreate our string list}
    StringList:=TStringList.Create;
    StringList.Add('ASC & Hex key');
    StringList.Add(MyResult);
    SetString(MyResult, PAnsiChar(Crypt), Length(MyData));
    StringList.Add(MyResult);
    
    {And use LoadFromStream to read it}
    WriteLn('Loading the TStringList from the file');
    StringList.LoadFromStream(FileStream);

    {Iterate the strings and print them to the screen}
    {
    WriteLn('The contents of the file are:');
    for Count:=0 to StringList.Count - 1 do
     begin
      WriteLn(StringList.Strings[Count]);
     end;
    }
    {Close the file and free the string list again}
    WriteLn('Closing the file');
    WriteLn('');
    FileStream.Free;
    StringList.Free;

    {If you remove the SD card and put in back in your computer, you should see the
     file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
     see the contents exactly as they appeared on screen.}
   except
    {TFileStream couldn't open the file}
    WriteLn('Failed to open the file ' + Filename);
   end;
  except
   {Something went wrong creating the file}
   WriteLn('Failed to create the file ' + Filename);
  end;
end;
procedure TTCPThread.ProcessingData(procSock: TSocket; Data: string);
begin
  if data <> '' then

   begin
   WriteLn(data+#32+'we get it from '+IntToStr(number)+' thread');
   //ProcessFpga(Data,TTCPThread.WindowHandle);
   //WriteLn('Calling ProcessFpga');
   ProcessEncryptDecrypt(Data);
   WriteLn(data);
   end;
end;


 var
   Server: TListenerThread;
begin
   Server:=TListenerThread.Create;
   ReadLn;
end.
.

