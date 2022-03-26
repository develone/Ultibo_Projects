program FileHandling;

{$mode objfpc}{$H+}

{ Example 08 File Handling                                                     }
{                                                                              }
{  This example demonstrates just a few basic functions of file handling which }
{  is a major topic in itself.                                                 }
{                                                                              }
{  For more information on all of the available file function see the Ultibo   }
{  Wiki or the Free Pascal user guide.                                         }
{                                                                              }
{  To compile the example select Run, Compile (or Run, Build) from the menu.   }
{                                                                              }
{  Once compiled select Tools, Run in QEMU ... from the Lazarus menu to launch }
{  the application in a QEMU session.                                          }
{                                                                              }
{  QEMU VersatilePB version                                                    }
{   What's the difference? See Project, Project Options, Config and Target.    }

{Declare some units used by this example.}
uses
  QEMUVersatilePB,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  Classes,
  uTFTP,
  Winsock2,
  { needed for telnet }
  Shell,
  ShellFilesystem,
  ShellUpdate,
  RemoteShell,
  Framebuffer,
  SysUtils,
  uReadBin,
  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC;         {Include the MMC/SD core to access our SD card}
 type
 Buffer = array[0..4159] of Char;
 BufPtr = ^Buffer;
{A window handle plus a couple of others.}
var
 Count:Integer;
 aFilename:String;

 SearchRec:TSearchRec;
 aStringList:TStringList;
 FileStream:TFileStream;

 WindowHandle:TWindowHandle;
 B : Buffer;
 BP : BufPtr;
 PP : Pointer;
 i,j: integer;
 Characters : String;
 CRC : String;
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

  ConsoleWindowWriteLn (WindowHandle, s);

end;


begin
 {Create our window}
 WindowHandle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);

 {Output the message}
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 08 File Handling');
 ConsoleWindowWriteLn(WindowHandle,'');

 {We may need to wait a couple of seconds for any drive to be ready}
 ConsoleWindowWriteLn(WindowHandle,'Waiting for drive C:\');
 while not DirectoryExists('C:\') do
  begin
   {Sleep for a moment}
   Sleep(100);
  end;
 ConsoleWindowWriteLn(WindowHandle,'C:\ drive is ready');
 ConsoleWindowWriteLn(WindowHandle,'');

 {First let's list the contents of the SD card. We can guess that it will be C:\
  drive because we didn't include the USB host driver.}
 ConsoleWindowWriteLn(WindowHandle,'Contents of drive C:\');

 {To list the contents we need to use FindFirst/FindNext, start with FindFirst}
 if FindFirst('C:\*.*',faAnyFile,SearchRec) = 0 then
  begin
   {If FindFirst succeeds it will return 0 and we can proceed with the search}
   repeat
    {Print the file found to the screen}
    ConsoleWindowWriteLn(WindowHandle,'Filename is ' + SearchRec.Name + ' - Size is ' + IntToStr(SearchRec.Size) + ' - Time is ' + DateTimeToStr(FileDateToDateTime(SearchRec.Time)));

   {We keep calling FindNext until there are no more files to find}
   until FindNext(SearchRec) <> 0;
  end;

 {After any call to FindFirst, you must call FindClose or else memory will be leaked}
 FindClose(SearchRec);
 ConsoleWindowWriteLn(WindowHandle,'');


   {If you remove the SD card and put in back in your computer, you should see the
    file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
    see the contents exactly as they appeared on screen.}

 aFilename:='C:\bb.bin';
 setFileName(aFileName);
 SetStingList(aStringList);
 i:=0;

 while (i < 4160) do
    	 begin
         Characters:=Readit(i);
         ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+Characters);
          i:=i+65;
         end;


 {Halt the thread}
 ThreadHalt(0);
end.

