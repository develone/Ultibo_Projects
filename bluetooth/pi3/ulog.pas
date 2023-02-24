unit uLog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TLogProc = procedure (s : string);

var
  FLogProc : TLogProc = nil;
  FLog3Proc : TLogProc = nil;

procedure SetLogProc (lp : TLogProc);
procedure Log (s : string);

procedure SetLog3Proc (lp : TLogProc);
procedure Log3 (s : string);

implementation

procedure SetLogProc (lp : TLogProc);
begin
  FLogProc := lp;
end;

procedure SetLog3Proc (lp : TLogProc);
begin
  FLog3Proc := lp;
end;

procedure Log (s : string);
begin
  if Assigned (FLogProc) then FLogProc (s);
end;

procedure Log3 (s : string);
begin
  if Assigned (FLog3Proc) then FLog3Proc (s);
end;

end.

