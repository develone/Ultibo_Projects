program lazliltest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazliltestmainunit, fplilpackage
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='Lazarus LIL Test';
  Application.Initialize;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.

