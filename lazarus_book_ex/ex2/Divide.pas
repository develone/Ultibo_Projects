program Divide;
{$mode objfpc}{$H+}
 
 
uses
  SysUtils,Classes;
  
type
  TDivide = class
    private 
    numerator,denominator,result,remainder:integer;
    procedure calculate;
    procedure displayresult;
  end;

procedure TDivide.calculate;
begin
  result:=(numerator div denominator);
  remainder:=(numerator mod denominator);
end;

procedure TDivide.displayresult;
begin
  WriteLn('result = ' + IntToStr(result));
  WriteLn('remainder = ' + IntToStr(remainder));
end;
 
var
Divide1 : TDivide;


begin
  { Add your program code here }
  Divide1 := TDivide.create;
  Divide1.numerator := 76;
  Divide1.denominator := 8;
  Divide1.calculate;
  Divide1.displayresult;
  Divide1.Free;
   
end.  
