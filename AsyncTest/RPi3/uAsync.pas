unit uAsync;

{$mode objfpc}
{$H+}

{ Thread based Async TCP Socket

  Ultibo (C) 2015 - SoftOz Pty Ltd.                        LGPLv2.1 with static linking exception
  FPC (c) 1993-2015 Free Pascal Team.                      Modified Library GNU Public License
  Lazarus (c) 1993-2015 Lazarus and Free Pascal Team.      Modified Library GNU Public License

  Other bits (c) 2016 pjde                                 LGPLv2.1 with static linking exception
}

interface

uses
  Classes, SysUtils, Winsock2, SyncObjs;

const

  stInit             = 0;
  stClosed           = 1;
  stConnecting       = 2;
  stConnected        = 3;
  stClosing          = 4;
  stError            = 5;
  stQuitting         = 6;


type

  { TAsyncSocket }

  TAsyncEvent = procedure (Sender : TObject);
  TReadEvent = procedure (Sender : TObject; Buff : pointer; BuffSize : integer);
  TMsgEvent = procedure (Sender : TObject; s : string);

  TAsyncSocket = class (TThread)
  private
    FOnConnect: TAsyncEvent;
    FOnClose: TAsyncEvent;
    FOnMsg: TMsgEvent;
    FOnRead: TReadEvent;
    FPort: Word;
    FAddr : string;
    FState : integer;
    FEvent : TEvent;
  protected
    procedure Execute; override;
    procedure SetState (NewState : integer);
    procedure DoMsg (s : string);
  public
    Socket : TWinsock2TCPClient;
    constructor Create (Msgs : TMsgEvent);
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Send (Buff : pointer; BuffSize : integer); overload;
    procedure Send (s : string); overload;
    function State : integer;
    property Addr : string read FAddr write FAddr;
    property Port : Word read FPort write FPort;
    property OnConnect : TAsyncEvent read FOnConnect write FOnConnect;
    property OnClose : TAsyncEvent read FOnClose write FOnClose;
    property OnRead : TReadEvent read FOnRead write FOnRead;
    property OnMsg : TMsgEvent read FOnMsg write FOnMsg;
  end;

function StateToStr (s : integer) : string;

implementation

function StateToStr (s : integer) : string;
begin
  case s of
    stInit       : Result := 'Init';
    stClosed     : Result := 'Closed';
    stConnecting : Result := 'Connecting';
    stConnected  : Result := 'Connected';
    stClosing    : Result := 'Closing';
    stError      : Result := 'Error';
    stQuitting   : Result := 'Quitting';
    else           Result := 'Unknown ' + IntToStr (s);
  end;
end;

{ TAsyncSocket }

procedure TAsyncSocket.Execute;
//const
//  ft : array [boolean] of string = ('FALSE', 'TRUE');
var
  Buff : array [0..255] of byte;
  closed : boolean;
  count : integer;
  res : boolean;
begin
  while not Terminated do
    begin
      if Socket = nil then Terminate;
      case FState of
        stClosed :
          begin
            FEvent.ResetEvent;
            FEvent.WaitFor (INFINITE); // park thread
          end;
        stConnecting :
           begin
            Socket.RemotePort := FPort;
            Socket.RemoteAddress := FAddr;
            if Socket.Connect then SetState (stConnected) else SetState (stClosed);
          end;
        stConnected :
          begin
            count := 0;
            closed := false;
            res := Socket.ReadAvailable (@Buff[0], 256, count, closed);
//           DoMsg ('Read Result ' + ft[res]);
            if res then
              begin
                if Assigned (FOnRead) then FOnRead (Self, @Buff[0], count);
              end;
            if not res or closed then SetState (stClosed);
          end;
        stQuitting : Terminate;
        end;  // case
    end;
end;

procedure TAsyncSocket.SetState (NewState: integer);
begin
  if (FState <> NewState) then
    begin
      DoMsg (StateToStr (FState) + ' to ' + StateToStr (NewState) + '.');
      case NewState of
        stClosed    : if Assigned (FOnClose) then FOnClose (Self);
        stConnected : if Assigned (FOnConnect) then FOnConnect (Self);
      end;
    end;
  FState := NewState;
  if not (FState in [stClosed]) then FEvent.SetEvent;
end;

procedure TAsyncSocket.DoMsg (s: string);
begin
  if Assigned (FOnMsg) then FOnMsg (Self, s);
end;

constructor TAsyncSocket.Create (Msgs : TMsgEvent);
begin
  inherited Create (false);
  FreeOnTerminate := true;
  FonMsg := Msgs;
  Socket := TWinsock2TCPClient.Create;
  FEvent := TEvent.Create (nil, true, false, '');
  FState := stInit;
  Start;
end;

destructor TAsyncSocket.Destroy;
begin
  FEvent.SetEvent;
  FEvent.Free;
  Socket.Free;
  Socket := nil;
  inherited Destroy;
end;

procedure TAsyncSocket.Connect;
begin
  if FState in [stClosed, stInit] then SetState (stConnecting);
end;

procedure TAsyncSocket.Disconnect;
begin
  if FState in [stConnected, stConnecting] then Socket.Disconnect;
end;

procedure TAsyncSocket.Send (Buff: pointer; BuffSize: integer);
begin
  if FState in [stConnected] then
    if not Socket.WriteData (Buff, BuffSize) then Socket.Disconnect;
end;

procedure TAsyncSocket.Send (s: string);
begin
  if FState in [stConnected] then
    if not Socket.WriteData (@s[1], length (s)) then Socket.Disconnect;
end;

function TAsyncSocket.State: integer;
begin
  Result := FState;
end;

end.

