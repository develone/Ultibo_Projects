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
uTFTP,Winsock2,
Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell;
{ needed for telnet }

  Syscalls;
   }
RaspberryPi3, GlobalConst, GlobalTypes, GlobalConfig, Platform, HeapManager,
Console, SysUtils, Threads, VC4, Syscalls, Mouse, keyboard, DWCOTG,

HTTP, WebStatus,uTFTP,Winsock2, Shell, ShellFilesystem, ShellUpdate,
RemoteShell,       
  



Framebuffer;
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
  LeftWindow:TWindowHandle;
  HTTPListener:THTTPListener;
 { needed to use ultibo-tftp  }
 TCP : TWinsock2TCPClient;
 IPAddress : string;


  function WaitForIPComplete : string;

 var

   TCP : TWinsock2TCPClient;

 begin

   TCP := TWinsock2TCPClient.Create;

   Result := TCP.LocalAddress;

   if (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') then

     begin

       while (Result = '') or (Result = '0.0.0.0') or (Result = '255.255.255.255') do

         begin

           sleep (1500);

           Result := TCP.LocalAddress;

         end;

     end;

   TCP.Free;

 end;
  procedure Msg (Sender : TObject; s : string);

 begin

   ConsoleWindowWriteLn (LeftWindow, s);

 end;
  procedure WaitForSDDrive;

 begin

   while not DirectoryExists ('C:\') do sleep (500);

 end;


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
 LeftWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Create and start the HTTP Listener for our web status page}
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;

 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);
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

