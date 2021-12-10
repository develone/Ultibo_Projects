program Crypto_WiFI_RPi3;

{$mode objfpc}{$H+}
{define RPI3}
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

  crypto,
  APICrypto,
  Winsock2,
  framebuffer,
  font,
  wifidevice,
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

const
   // copied from font as it's in the implementation section there.
   FONT_LATIN1_8X16_DATA:TFontData8x16 = (
    Data:(($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $18, $3C, $3C, $3C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00),
          ($00, $66, $66, $66, $24, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $6C, $FE, $6C, $6C, $6C, $FE, $6C, $6C, $00, $00, $00, $00),
          ($00, $10, $10, $7C, $D6, $D0, $D0, $7C, $16, $16, $D6, $7C, $10, $10, $00, $00),
          ($00, $00, $00, $00, $C2, $C6, $0C, $18, $30, $60, $C6, $86, $00, $00, $00, $00),
          ($00, $00, $38, $6C, $6C, $38, $76, $DC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $18, $18, $18, $30, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $0C, $18, $30, $30, $30, $30, $30, $30, $18, $0C, $00, $00, $00, $00),
          ($00, $00, $30, $18, $0C, $0C, $0C, $0C, $0C, $0C, $18, $30, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $66, $3C, $FF, $3C, $66, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $18, $18, $7E, $18, $18, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $18, $30, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $FE, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $06, $0C, $18, $30, $60, $C0, $00, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $CE, $CE, $D6, $D6, $E6, $E6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $18, $38, $78, $18, $18, $18, $18, $18, $18, $7E, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $06, $0C, $18, $30, $60, $C0, $C6, $FE, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $06, $06, $3C, $06, $06, $06, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $0C, $1C, $3C, $6C, $CC, $FE, $0C, $0C, $0C, $1E, $00, $00, $00, $00),
          ($00, $00, $FE, $C0, $C0, $C0, $FC, $06, $06, $06, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $38, $60, $C0, $C0, $FC, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $FE, $C6, $06, $06, $0C, $18, $30, $30, $30, $30, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $7C, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $7E, $06, $06, $06, $0C, $78, $00, $00, $00, $00),
          ($00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $18, $18, $00, $00, $00, $18, $18, $30, $00, $00, $00, $00),
          ($00, $00, $00, $06, $0C, $18, $30, $60, $30, $18, $0C, $06, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $FE, $00, $00, $FE, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $60, $30, $18, $0C, $06, $0C, $18, $30, $60, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $0C, $18, $18, $18, $00, $18, $18, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $DE, $DE, $DE, $DC, $C0, $7C, $00, $00, $00, $00),
          ($00, $00, $10, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $FC, $66, $66, $66, $7C, $66, $66, $66, $66, $FC, $00, $00, $00, $00),
          ($00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $00, $00, $00, $00),
          ($00, $00, $F8, $6C, $66, $66, $66, $66, $66, $66, $6C, $F8, $00, $00, $00, $00),
          ($00, $00, $FE, $66, $62, $68, $78, $68, $60, $62, $66, $FE, $00, $00, $00, $00),
          ($00, $00, $FE, $66, $62, $68, $78, $68, $60, $60, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $3C, $66, $C2, $C0, $C0, $DE, $C6, $C6, $66, $3A, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $1E, $0C, $0C, $0C, $0C, $0C, $CC, $CC, $CC, $78, $00, $00, $00, $00),
          ($00, $00, $E6, $66, $66, $6C, $78, $78, $6C, $66, $66, $E6, $00, $00, $00, $00),
          ($00, $00, $F0, $60, $60, $60, $60, $60, $60, $62, $66, $FE, $00, $00, $00, $00),
          ($00, $00, $C6, $EE, $FE, $FE, $D6, $C6, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $FC, $66, $66, $66, $7C, $60, $60, $60, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $D6, $DE, $7C, $0C, $0E, $00, $00),
          ($00, $00, $FC, $66, $66, $66, $7C, $6C, $66, $66, $66, $E6, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $64, $38, $0C, $06, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $7E, $7E, $5A, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $6C, $38, $10, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $C6, $C6, $D6, $D6, $D6, $FE, $EE, $6C, $00, $00, $00, $00),
          ($00, $00, $C6, $C6, $6C, $7C, $38, $38, $7C, $6C, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $FE, $C6, $86, $0C, $18, $30, $60, $C2, $C6, $FE, $00, $00, $00, $00),
          ($00, $00, $3C, $30, $30, $30, $30, $30, $30, $30, $30, $3C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $C0, $60, $30, $18, $0C, $06, $00, $00, $00, $00, $00),
          ($00, $00, $3C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $0C, $3C, $00, $00, $00, $00),
          ($10, $38, $6C, $C6, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FF, $00),
          ($00, $30, $30, $30, $18, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $E0, $60, $60, $78, $6C, $66, $66, $66, $66, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $1C, $0C, $0C, $3C, $6C, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $38, $6C, $64, $60, $F0, $60, $60, $60, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $CC, $78, $00),
          ($00, $00, $E0, $60, $60, $6C, $76, $66, $66, $66, $66, $E6, $00, $00, $00, $00),
          ($00, $00, $18, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $06, $06, $00, $0E, $06, $06, $06, $06, $06, $06, $66, $66, $3C, $00),
          ($00, $00, $E0, $60, $60, $66, $6C, $78, $78, $6C, $66, $E6, $00, $00, $00, $00),
          ($00, $00, $38, $18, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $EC, $FE, $D6, $D6, $D6, $D6, $C6, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $DC, $66, $66, $66, $66, $66, $7C, $60, $60, $F0, $00),
          ($00, $00, $00, $00, $00, $76, $CC, $CC, $CC, $CC, $CC, $7C, $0C, $0C, $1E, $00),
          ($00, $00, $00, $00, $00, $DC, $76, $66, $60, $60, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $60, $38, $0C, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $10, $30, $30, $FC, $30, $30, $30, $30, $36, $1C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $66, $66, $66, $66, $66, $3C, $18, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $C6, $C6, $D6, $D6, $D6, $FE, $6C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $C6, $6C, $38, $38, $38, $6C, $C6, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00),
          ($00, $00, $00, $00, $00, $FE, $CC, $18, $30, $60, $C6, $FE, $00, $00, $00, $00),
          ($00, $00, $0E, $18, $18, $18, $70, $18, $18, $18, $18, $0E, $00, $00, $00, $00),
          ($00, $00, $18, $18, $18, $18, $18, $18, $18, $18, $18, $18, $00, $00, $00, $00),
          ($00, $00, $70, $18, $18, $18, $0E, $18, $18, $18, $18, $70, $00, $00, $00, $00),
          ($00, $00, $76, $DC, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $7E, $C3, $99, $99, $F3, $E7, $E7, $FF, $E7, $E7, $7E, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $82, $FE, $00, $00, $00, $00),
          ($00, $00, $00, $00, $18, $18, $00, $18, $18, $18, $3C, $3C, $3C, $18, $00, $00),
          ($00, $00, $00, $00, $10, $7C, $D6, $D0, $D0, $D0, $D6, $7C, $10, $00, $00, $00),
          ($00, $00, $38, $6C, $60, $60, $F0, $60, $60, $66, $F6, $6C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $C6, $7C, $6C, $6C, $7C, $C6, $00, $00, $00, $00, $00, $00),
          ($00, $00, $66, $66, $3C, $18, $7E, $18, $7E, $18, $18, $18, $00, $00, $00, $00),
          ($00, $00, $18, $18, $18, $18, $00, $18, $18, $18, $18, $18, $00, $00, $00, $00),
          ($00, $7C, $C6, $60, $38, $6C, $C6, $C6, $6C, $38, $0C, $C6, $7C, $00, $00, $00),
          ($00, $6C, $6C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $3C, $42, $99, $A5, $A1, $A5, $99, $42, $3C, $00, $00, $00, $00, $00),
          ($00, $00, $3C, $6C, $6C, $3E, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $36, $6C, $D8, $6C, $36, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $FE, $06, $06, $06, $06, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $7E, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $3C, $42, $B9, $A5, $B9, $A5, $A5, $42, $3C, $00, $00, $00, $00, $00),
          ($FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $38, $6C, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $18, $18, $7E, $18, $18, $00, $7E, $00, $00, $00, $00),
          ($38, $6C, $18, $30, $7C, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($38, $6C, $18, $6C, $38, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $18, $30, $60, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $CC, $CC, $CC, $CC, $CC, $CC, $F6, $C0, $C0, $C0, $00),
          ($00, $00, $7F, $D6, $D6, $76, $36, $36, $36, $36, $36, $36, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $18, $18, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $18, $6C, $38, $00),
          ($30, $70, $30, $30, $78, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $38, $6C, $6C, $38, $00, $7C, $00, $00, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $D8, $6C, $36, $6C, $D8, $00, $00, $00, $00, $00, $00),
          ($60, $E0, $60, $60, $F6, $0C, $18, $30, $66, $CE, $1A, $3F, $06, $06, $00, $00),
          ($60, $E0, $60, $60, $F6, $0C, $18, $30, $6E, $DB, $06, $0C, $1F, $00, $00, $00),
          ($70, $D8, $30, $D8, $76, $0C, $18, $30, $66, $CE, $1A, $3F, $06, $06, $00, $00),
          ($00, $00, $00, $00, $30, $30, $00, $30, $30, $30, $60, $C6, $C6, $7C, $00, $00),
          ($60, $30, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($0C, $18, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($10, $38, $6C, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00),
          ($76, $DC, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $6C, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $C6, $00, $00, $00, $00),
          ($38, $6C, $38, $00, $38, $6C, $C6, $C6, $FE, $C6, $C6, $C6, $00, $00, $00, $00),
          ($00, $00, $3E, $78, $D8, $D8, $FC, $D8, $D8, $D8, $D8, $DE, $00, $00, $00, $00),
          ($00, $00, $3C, $66, $C2, $C0, $C0, $C0, $C0, $C2, $66, $3C, $0C, $66, $3C, $00),
          ($60, $30, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
          ($0C, $18, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
          ($10, $38, $6C, $00, $FE, $66, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
          ($00, $6C, $00, $FE, $66, $60, $60, $7C, $60, $60, $66, $FE, $00, $00, $00, $00),
          ($60, $30, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $08, $00, $00, $00),
          ($06, $0C, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($18, $3C, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $66, $00, $3C, $18, $18, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $F8, $6C, $66, $66, $F6, $66, $66, $66, $6C, $F8, $00, $00, $00, $00),
          ($76, $DC, $00, $C6, $E6, $F6, $FE, $DE, $CE, $C6, $C6, $C6, $00, $00, $00, $00),
          ($60, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($0C, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($10, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $66, $3C, $18, $3C, $66, $00, $00, $00, $00, $00, $00),
          ($00, $00, $7E, $C6, $CE, $CE, $DE, $F6, $E6, $E6, $C6, $FC, $00, $00, $00, $00),
          ($60, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($0C, $18, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($10, $38, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($06, $0C, $00, $66, $66, $66, $66, $3C, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $F0, $60, $7C, $66, $66, $66, $66, $7C, $60, $F0, $00, $00, $00, $00),
          ($00, $00, $7C, $C6, $C6, $C6, $CC, $C6, $C6, $C6, $D6, $DC, $80, $00, $00, $00),
          ($00, $60, $30, $18, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $18, $30, $60, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $10, $38, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $76, $DC, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $38, $6C, $38, $00, $78, $0C, $7C, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7E, $DB, $1B, $7F, $D8, $DB, $7E, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7C, $C6, $C0, $C0, $C0, $C6, $7C, $18, $6C, $38, $00),
          ($00, $60, $30, $18, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $0C, $18, $30, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $10, $38, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $00, $7C, $C6, $FE, $C0, $C0, $C6, $7C, $00, $00, $00, $00),
          ($00, $60, $30, $18, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $0C, $18, $30, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $18, $3C, $66, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $00, $38, $18, $18, $18, $18, $18, $3C, $00, $00, $00, $00),
          ($00, $78, $30, $78, $0C, $7E, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $76, $DC, $00, $DC, $66, $66, $66, $66, $66, $66, $00, $00, $00, $00),
          ($00, $60, $30, $18, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $0C, $18, $30, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $10, $38, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $76, $DC, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $6C, $00, $7C, $C6, $C6, $C6, $C6, $C6, $7C, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $18, $00, $7E, $00, $18, $00, $00, $00, $00, $00, $00),
          ($00, $00, $00, $00, $00, $7E, $CE, $DE, $FE, $F6, $E6, $FC, $00, $00, $00, $00),
          ($00, $60, $30, $18, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $18, $30, $60, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $30, $78, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $00, $00, $CC, $00, $CC, $CC, $CC, $CC, $CC, $CC, $76, $00, $00, $00, $00),
          ($00, $0C, $18, $30, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00),
          ($00, $00, $F0, $60, $60, $7C, $66, $66, $66, $66, $7C, $60, $60, $F0, $00, $00),
          ($00, $00, $00, $6C, $00, $C6, $C6, $C6, $C6, $C6, $C6, $7E, $06, $0C, $F8, $00))
    );


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
SSID : string;
  key : string;
  Country : string;
  topwindow : THandle;
  Winsock2TCPClient : TWinsock2TCPClient;
  IPAddress : string;
  i : integer;
  HTTPListener : THTTPListener;
  ScanResultList : TStringList;
  Status : Longword;
  CYW43455Network: PCYW43455Network;
  BSSIDStr : string;
  BSSID : ether_addr;
{0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb7 0xb0 0xb0 0xb0 0xb7 0x8a < A1009W70007
0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb3 0xb0 0xb0 0xb0 0xb3 0x8a < A1009W30003
0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb1 0xb0 0xb0 0xb0 0xb1 0x8a < A1009W10001

0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb7 0xb0 0xb0 0xb0 0xb0 0x8a < A1009W70000
0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb3 0xb0 0xb0 0xb0 0xb0 0x8a < A1009W30000
0xc1 0xb1 0xb0 0xb0 0xb9 0xd7 0xb1 0xb0 0xb0 0xb0 0xb0 0x8a < A1009W10000}
  procedure DumpIP;
var
  i, j, c : integer;
  s : string;
begin
  for i := 0 to 15 do
  begin
    s := '';
    for c := 1 to length(ipaddress) do
    begin
      for j := 7 downto 0 do
      begin
        if (FONT_LATIN1_8X16_DATA.data[ord(ipaddress[c]), i] and (1 shl j) = (1 shl j)) then
          s := s + '#'
        else
          s := s + ' ';
      end;
    end;
    consolewindowwriteln(topwindow, s);
  end;
end;
  procedure WaitForIP;
begin
  Winsock2TCPClient:=TWinsock2TCPClient.Create;

  while (true) do
  begin
    sleep(200);
    if (Winsock2TCPClient.LocalAddress <> IPAddress)
       and (length(Winsock2TCPClient.LocalAddress) > 0)
       and (Winsock2TCPClient.LocalAddress <> ' ') then
    begin
      ConsoleWindowWriteLn(topwindow, 'IP address='+Winsock2TCPClient.LocalAddress);
      IPAddress := Winsock2TCPClient.LocalAddress;
      break;
    end;
  end;
end;

  procedure WIFIScanCallback(ssid : string; ScanResultP : pwl_escan_result);
  var
    ssidstr : string;
  begin
    ssidstr := ssid + ' ' + inttohex(ScanResultP^.bss_info[1].BSSID.octet[0],2) + ':'
                              + inttohex(ScanResultP^.bss_info[1].BSSID.octet[1],2) + ':'
                              + inttohex(ScanResultP^.bss_info[1].BSSID.octet[2],2) + ':'
                              + inttohex(ScanResultP^.bss_info[1].BSSID.octet[3],2) + ':'
                              + inttohex(ScanResultP^.bss_info[1].BSSID.octet[4],2) + ':'
                              + inttohex(ScanResultP^.bss_info[1].BSSID.octet[5],2);

    if (ScanResultList <> nil) and (ScanResultList.Indexof(ssidstr) < 0) then
      ScanResultList.Add(ssidstr);
  end;


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
  ConsoleFramebufferDeviceAdd(FramebufferDeviceGetDefault);

  topwindow := ConsoleWindowCreate(ConsoleDeviceGetDefault, CONSOLE_POSITION_TOPLEFT,TRUE);
  WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);



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
  SSID : string;
  wifikey : string;
  Country : string;


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


 var
   Server: TListenerThread;
begin
   Server:=TListenerThread.Create;
   ReadLn;
end.
.

