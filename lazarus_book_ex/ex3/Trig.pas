program Trig;
{$mode objfpc}{$H+}
 
 
uses
  SysUtils,Classes;
  
type
  TTrig = class
    float : single;
    hyp : single;
    function getres():single;
    
  end;

function TTrig.getres:single;
begin
  hyp:=100;
  //The cos(60) = 0.5
  float:= Cos(Pi/3)*hyp;  //180/3 = 60 degrees
  getres:= float;
end;
 
var
Trig1 : TTrig;


begin
  { Add your program code here }
  Trig1 := TTrig.create;
  Trig1.getres;
  WriteLn('cos(60)*hyp = ' + FloatToStr(Trig1.float));
  Trig1.Free;
   
end.  
