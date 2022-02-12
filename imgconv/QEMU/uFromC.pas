unit uFromC;

{$mode objfpc}{$H+}
{$linklib cvtutils}
interface
uses
  GlobalConfig,
  GlobalConst;

procedure ReturnFromProcessStr(Value: PChar); cdecl; public name 'returnfromprocessstr';

implementation

procedure ReturnFromProcessStr(Value: PChar); cdecl;


begin
  // Do something with the value supplied
  writeLn('In pascal ReturnFromProcessStr calling calling  C returnfromprocessstr');
end;

end.
