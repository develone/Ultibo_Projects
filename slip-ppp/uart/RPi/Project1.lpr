program Project1;

{$mode objfpc}{$H+}

{ Raspberry Pi Zero Application                                                }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Ultibo,
  { Add additional units here }
  Console,
  { unit to allow upload of new kernel.img (or any other file) to SD }
  serialupdater
  ;

var
  WindowHandle: TWindowHandle;
  alongword: longword=0;

begin
  { Add your program code here }
  WindowHandle:= ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_LEFT, true);
  while true do
  begin
    ConsoleWindowClear(WindowHandle);
    ConsoleWindowWriteLn (WindowHandle, 'Hello World '+inttostr(alongword));
    inc(alongword);
    sleep(1000);
  end;


end.

