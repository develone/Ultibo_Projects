program SimpleObjectPascalProgram;
{$mode objfpc}{$H+}
 
type
  THelloWorld = class
    procedure WriteOut;
  end;

  TThisWorld = class
    procedure WriteOut2;
  end;

procedure THelloWorld.WriteOut;
begin
  WriteLn('Hello, World!');
end;

procedure TThisWorld.WriteOut2;
begin
  WriteLn('This is it');
end;

var
HelloWorld : THelloWorld;
ThisWorld : TThisWorld;

begin
  { Add your program code here }
  HelloWorld := THelloWorld.create;
  ThisWorld := TThisWorld.create;

  HelloWorld.WriteOut;
  ThisWorld.WriteOut2;

  HelloWorld.Free;
  ThisWorld.Free;
end.  
