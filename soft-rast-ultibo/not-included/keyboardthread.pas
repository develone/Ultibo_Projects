unit KeyboardThread;

{$mode objfpc}{$H+}

interface

uses
  //Classes, SysUtils,Threads, keyboard;
 GlobalConst, GlobalConfig, Platform, HeapManager,
Console, SysUtils, Threads, VC4, Syscalls, Mouse, keyboard, DWCOTG, Framebuffer;

//procedure NonBlockingKeyboard;
function Thread1Execute(Parameter:Pointer):PtrInt;

implementation

var
 KeyPressed            : Char;
 KeyDown               : Integer;


procedure getKey(var value : Char; var down : integer); export; cdecl;
begin
   value := KeyPressed;
   down  := KeyDown;
end;

//procedure NonBlockingKeyboard;
function Thread1Execute(Parameter:Pointer):PtrInt;
var
 //Count:LongWord;
 //Data:TKeyboardData;
 Character:Char;
begin
   ConsoleGetKey(Character,nil);
   KeyPressed := Character;
  {*
  KeyDown := 0;
  KeyPressed := 0;
  Count := 1;

  if KeyboardReadEx(@Data, SizeOf(TKeyboardData), KEYBOARD_FLAG_NONE, Count) = ERROR_SUCCESS then
  begin
    if (Data.KeyCode = KEY_CODE_F12) then SystemRestart(0);

    KeyPressed := Data.ScanCode;
    if (Data.Modifiers AND KEYBOARD_KEYUP) = 0 then
      KeyDown := 1;
  end;
  *}
end;

end.

