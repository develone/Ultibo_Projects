program project1;

{$mode objfpc}{$H+}

{ Raspberry Pi 3 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  {
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Classes,
  Framebuffer,
  Ultibo,
  Math,
  Syscalls;
   }
RaspberryPi3, GlobalConst, GlobalTypes, GlobalConfig, Platform, HeapManager,
Console, SysUtils, Threads, VC4, Syscalls, Mouse, keyboard, DWCOTG, Framebuffer;

{$linklib Testnanogl}
{$linklib t3dlib1}
{$linklib t3dlib4}
{$linklib t3dlib5}
{$linklib t3dlib6}
{$linklib t3dlib7}
{$linklib t3dlib8}
{$linklib t3dlib9}
{$linklib t3dlib10}
{$linklib t3dlib11}
{$linklib t3dlib12}
{$linklib t3dlib13}


procedure test; cdecl; external 'libTestnanogl' name 'test';


  { Add additional units here }


var
  FramebufferDevice     : PFramebufferDevice;
  FramebufferProperties : TFramebufferProperties;
  BufferStart           : Pointer;
  PageSize              : Integer;
  CurrentPage           : Integer;


  GfxWidth, GfxHeight   : Integer;




  {$IFDEF PLATFORM_QEMU}
   const RUNDELAY = 100;
  {$ELSE}
   const RUNDELAY = 200;
  {$ENDIF}

  function getScreenPitch() : integer; export; cdecl; //USED
  begin
     Result := FramebufferProperties.Pitch;
  end;

  function getScreenBuffer() : pointer; export; cdecl; //USED
  begin
     Result := BufferStart;
  end;

  procedure getKey(var value : integer); export; cdecl;
var
  Key: Char;
begin
  // Check if a key is available (without waiting)
  if ConsolePeekKey(Key, nil) then
  begin
    // Remove the key from the buffer
    ConsoleGetKey(Key, nil);

    // Return the ordinal value of the character
    value := Ord(Key);
  end
  else
    value := -1;
end;



 procedure getScreenSize(var scrWidth: LongWord ;  var scrHeight: LongWord);export; cdecl;
  begin
   scrWidth  := FramebufferProperties.PhysicalWidth;
   scrHeight := FramebufferProperties.PhysicalHeight;
  end;


begin
  { Add your program code here }
  ThreadSetCPU(ThreadGetCurrent, CPU_ID_3);
  Sleep(RUNDELAY);

  // This breaks framebuffer ininitialisation into two parts because it's suggested that
  // you sleep between allocating the buffer and getting its properties, but seeing as
  // we need to wait for the disk to be available, we can effectively cut out a wait
  // and use one alone the way.
   FramebufferDevice := FramebufferDeviceGetDefault;
   FramebufferDeviceGetProperties(FramebufferDevice, @FramebufferProperties);
   FramebufferDeviceRelease(FramebufferDevice);
   Sleep(RUNDELAY);
   FramebufferProperties.Depth := 16;
   FramebufferProperties.VirtualWidth:= FramebufferProperties.PhysicalWidth;
   FramebufferProperties.VirtualHeight := FramebufferProperties.PhysicalHeight*2;

   FRAMEBUFFER_CONSOLE_AUTOCREATE := False;
   FramebufferDeviceAllocate(FramebufferDevice, @FramebufferProperties);

  while not DirectoryExists('C:\') do
    begin
      Sleep(100);
    end;

  FramebufferDeviceGetProperties(FramebufferDevice, @FramebufferProperties);
  BufferStart := Pointer(FramebufferProperties.Address);
  PageSize := FramebufferProperties.Pitch * FramebufferProperties.PhysicalHeight;
  CurrentPage := 0;
  //Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

  test; {main thread}

  ThreadHalt(0);
end.

