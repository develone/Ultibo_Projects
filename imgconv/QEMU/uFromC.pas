unit uFromC;

{$mode objfpc}{$H+}
{$linklib cvtutils}
interface
uses
  GlobalConfig,
  GlobalConst;

var
  ProcessStrResult:String;

procedure ReturnFromProcessStr(Value: PChar); cdecl; public name 'returnfromprocessstr';

implementation

procedure ReturnFromProcessStr(Value: PChar); cdecl;


begin
  // Do something with the value supplied
  writeLn('In pascal ReturnFromProcessStr calling calling  C returnfromprocessstr');

  // Print the value to the console
  writeLn('Value from C is ' + Value);

  // Save the value to a variable so it can be used elsewhere
  ProcessStrResult := Value;
end;

end.

