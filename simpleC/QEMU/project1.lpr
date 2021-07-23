program project1;

{$mode objfpc}{$H+}

{ QEMU VersatilePB Application                                                 }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  QEMUVersatilePB,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Framebuffer,
  SysUtils,
  Classes,
  Syscalls,
  Ultibo;
{$linklib simpleC}
procedure simpleC; cdecl; external 'libsimpleC' name 'simpleC';

{We'll need a window handle plus a couple of others.}
var
 Count:LongWord;
 Character:Char;
 Characters:String;
 WindowHandle:TWindowHandle;

begin
 {Create a console window at full size}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Output some welcome text on the console window}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example SimpleC');
 ConsoleWindowWriteLn(WindowHandle,'Calling a C function from Ultibo');
 ConsoleWindowWriteLn(WindowHandle,'First execute ./buildlib.sh to create libsimpleC.a');
 ConsoleWindowWriteLn(WindowHandle,'Using Lazerus IDE (Ultibo Edition) compile project1.lpi');

 ConsoleWindowWriteLn(WindowHandle,'This will be linked with project1.lpi');
 ConsoleWindowWriteLn(WindowHandle,'qemu-system-arm -machine versatilepb -cpu cortex-a8 -kernel kernel.bin');
 simpleC();
end.

