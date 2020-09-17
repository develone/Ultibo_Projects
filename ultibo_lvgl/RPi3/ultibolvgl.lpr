program ultibolvgl;

{$mode objfpc}{$H+}

{ Raspberry Pi 3 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses

initunit,RaspberryPi3, GlobalConst, GlobalTypes, GlobalConfig, Platform, HeapManager,
Console, SysUtils, Threads, VC4, Syscalls, Mouse, keyboard, DWCOTG, Framebuffer;

{$linklib ultiboClvgl}


procedure lvglmain; cdecl; external 'libultiboClvgl' name 'lvglmain';

var
  FramebufferDevice     : PFramebufferDevice;
  FramebufferProperties : TFramebufferProperties;
  BufferStart           : Pointer;
  //PageSize              : Integer;
  //CurrentPage           : Integer;


  //GfxWidth, GfxHeight   : Integer;

  ThreadMouseHandle:TThreadHandle;
  mouseCX: integer = 0;
  mouseCY: integer = 0;
  mouseBt: integer = 0;


  {$IFDEF PLATFORM_QEMU}
   const RUNDELAY = 100;
  {$ELSE}
   const RUNDELAY = 200;
  {$ENDIF}

  function getScreenPitch() : integer; export; cdecl; 
  begin
     Result := FramebufferProperties.Pitch;
  end;

  function getScreenBuffer() : pointer; export; cdecl; 
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

 procedure getMouseXY(var cx: integer; var cy: integer; var btc: integer);export; cdecl;
  begin
   cx  := mouseCX;
   cy  := mouseCY;
   btc := mouseBt;
  end;

function NonBlockingMouse(Parameter:Pointer):PtrInt;
var
  Mousedata : TMouseData;
  count : LongWord;
  ScalingX:Double;
  ScalingY:Double;
  ScreenWidth:LongWord;
  ScreenHeight:LongWord;
begin
 ScreenWidth  := FramebufferProperties.PhysicalWidth;
 ScreenHeight := FramebufferProperties.PhysicalHeight;
 while True do
  begin
  {if (MousePeek() = ERROR_SUCCESS) then}

  if MouseRead(@MouseData,SizeOf(TMouseData),Count) = ERROR_SUCCESS then
   begin
    {We received a mouse message so let's process it to see what it contains.
    The TMouseData structure will give us an X an Y Offset as well as any buttons
    that are currently pressed.}

    {Check the buttons}
    if MouseData.Buttons = 0 then
     begin
      mouseBt:=0;
     end
    else
     begin
      if (MouseData.Buttons and (MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON)) = (MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON) then
       begin
        mouseBt:=3;
       end
      else if (MouseData.Buttons and MOUSE_LEFT_BUTTON) = MOUSE_LEFT_BUTTON then
       begin
        mouseBt:=1;
       end
      else if (MouseData.Buttons and MOUSE_RIGHT_BUTTON) = MOUSE_RIGHT_BUTTON then
       begin
        mouseBt:=2;
       end
      else
       begin
        mouseBt:=4;
       end;
     end;
     Result := 1;
   end;

   {Now update our mouse tracking for cursor X and Y}
         {Check if the X value is absolute instead of relative}
         if (MouseData.Buttons and MOUSE_ABSOLUTE_X) = MOUSE_ABSOLUTE_X then
          begin
           {For absolute values the maximum X field allows us to scale
            the cursor X value relative to the size of our screen}
           ScalingX:=MouseData.MaximumX / ScreenWidth;
           if ScalingX <= 0 then ScalingX:=1.0;

           mouseCX:=Trunc(MouseData.OffsetX / ScalingX);
          end
         else
          begin
           mouseCX:=mouseCX + MouseData.OffsetX;
          end;
         if mouseCX < 0 then mouseCX:=0;
         if mouseCX > (ScreenWidth - 1) then mouseCX:=ScreenWidth - 1;

         {Check if the Y value is absolute}
         if (MouseData.Buttons and MOUSE_ABSOLUTE_Y) = MOUSE_ABSOLUTE_Y then
          begin
           {Use maximum Y to scale the Y value to the screen}
           ScalingY:=MouseData.MaximumY / ScreenHeight;
           if ScalingY <= 0 then ScalingY:=1.0;

           mouseCY:=Trunc(MouseData.OffsetY / ScalingY);
          end
         else
          begin
           mouseCY:=mouseCY + MouseData.OffsetY;
          end;
         if mouseCY < 0 then mouseCY:=0;
         if mouseCY > (ScreenHeight - 1) then mouseCY:=ScreenHeight - 1;

end;
end;

begin
  { Add your program code here }
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
  FramebufferProperties.Depth := 32;
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
 //PageSize := FramebufferProperties.Pitch * FramebufferProperties.PhysicalHeight;
 //CurrentPage := 0;


 ThreadMouseHandle:=BeginThread(@NonBlockingMouse,nil,ThreadMouseHandle,THREAD_STACK_DEFAULT_SIZE);
 if ThreadMouseHandle = INVALID_HANDLE_VALUE then
   begin
    {If the thread handle is not valid then BeginThread failed}
    {ConsoleWindowWriteLn(WindowHandle,'Failed to create Thread1');}
   end
  else
   begin
    lvglmain; {main thread}
   end;

 ThreadHalt(0);
end.
