program Srv;

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
  SysUtils,
  BCM2836,
  BCM2709,
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
     {procedure ProcessFpga(Data: string;ProgWindow:TWindowHandle);}
     procedure ProcessFpga(Data: string);
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

    ConsoleWindowWriteLn(WindowHandle, 'AES GCM Encrypt/Decrypt test');
  ConsoleWindowWriteLn(WindowHandle, 'MyData is ' + MyData);

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
    ConsoleWindowWriteLn(WindowHandle, 'AES GCM Encrypt Success');

    {Clear the plain buffer}
    FillChar(Plain^, Length(MyData), 0);

    {Decrypt the Data}
    if AESGCMDecryptData(Key, Length(MyKey), IV, AAD, Crypt, Plain, Length(MyIV), Length(MyAAD), Length(MyData), Tag) then
    begin
      ConsoleWindowWriteLn(WindowHandle, 'AES GCM Decrypt Success');

      {Copy the result}
      SetString(MyResult, PAnsiChar(Plain), Length(MyData));

      ConsoleWindowWriteLn(WindowHandle, 'MyResult is ' + MyResult);
    end
    else
    begin
      ConsoleWindowWriteLn(WindowHandle, 'AES GCM Decrypt Failure');
    end;
  end
  else
  begin
    ConsoleWindowWriteLn(WindowHandle, 'AES GCM Encrypt Failure');
  end;


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
      WriteLn('Bind error: '+GetErrorDescEx);
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
              WriteLn('We have '+ IntToStr(FThreadManager.GetActiveThreadCount)+#32+'client threads!');
              end
             else
              WriteLn('TCP thread creation error: '+GetErrorDescEx);
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

{procedure TTCPThread.ProcessFpga(Data : string;ProgWindow:TWindowHandle);}
procedure TTCPThread.ProcessFpga(Data : string);
{123456789012345
 gpio 0x00010001
 gpio 0x00030003
 gpio 0x00070007
 gpio 0x00010009
 gpio 0x00020000
 gpio 0x00040000
 gpio 0x00070000}

var
  nr : integer;
  datab : Byte;
  bp : ^Byte;
  rdp : ^Byte;
  CmdArray : array [0..11] of Byte;
  RDArray : array [0..11] of Byte;
  cmd : string;
begin
  cmd := copy(Data,1,4);
  if (cmd = 'gpio') then
   begin
     WriteLn('in ProcessFpga');
     CmdArray[0] := 193; // 0xC1 0x41
     CmdArray[1] := 177; // 0xB1 0x31
     CmdArray[2] := 176; // 0xB0 0x30
     CmdArray[3] := 176; // 0xB0 0x30
     CmdArray[4] := 185; // 0xB9 0x39
     CmdArray[5] := 215; // 0xD7 0x57

     cmd := copy(Data,11,1);
     if (cmd ='1' ) then  CmdArray[6] := 177;
     if (cmd ='2' ) then  CmdArray[6] := 178;
     if (cmd ='3' ) then  CmdArray[6] := 179;
     if (cmd ='4' ) then  CmdArray[6] := 180;
     if (cmd ='6' ) then  CmdArray[6] := 182;
     if (cmd ='7' ) then  CmdArray[6] := 183;

     CmdArray[7] := 176; // 0xB0 0x30
     CmdArray[8] := 176; // 0xB0 0x30
     CmdArray[9] := 176; // 0xB0 0x30
     cmd := copy(Data,15,1);
     if (cmd ='0' ) then  CmdArray[10] := 176;
     if (cmd ='1' ) then  CmdArray[10] := 177;
     if (cmd ='2' ) then  CmdArray[10] := 178;
     if (cmd ='3' ) then  CmdArray[10] := 179;
     if (cmd ='4' ) then  CmdArray[10] := 180;
     if (cmd ='6' ) then  CmdArray[10] := 182;
     if (cmd ='7' ) then  CmdArray[10] := 183;
     CmdArray[11] := 138;
     nr := 11;
     bp := @CmdArray;
     rdp := @RDArray;;
     for nr := 0 to nr do
      begin
      datab := bp^;
      inc(bp);

      WriteLn('using ptr '+inttostr(nr) + ' '+ inttostr(datab));
     end;
     bp := @CmdArray;
     WriteFpga1(bp,nr,rdp);
   end;
  if data <> '' then
    WriteLn(data);
end;

procedure TTCPThread.ProcessingData(procSock: TSocket; Data: string);
begin
  if data <> '' then

   begin
   WriteLn(data+#32+'we get it from '+IntToStr(number)+' thread');
   //ProcessFpga(Data,TTCPThread.WindowHandle);
   WriteLn('Calling ProcessFpga');
   ProcessFpga(Data);
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

