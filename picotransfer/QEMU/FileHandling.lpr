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

  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC;         {Include the MMC/SD core to access our SD card}
 type
 Buffer = array[0..4159] of Char;
 BufPtr = ^Buffer;
{A window handle plus a couple of others.}
var
 Count:Integer;
 Filename:String;
 Filenamelog:String;

 SearchRec:TSearchRec;
 StringList,StringListlog:TStringList;
 FileStream:TFileStream;
 FileStreamlog:TFileStream;
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

 {Let's try creating a file and writing some text to it, we'll assign our filename
  to a variable.}
 Filename:='C:\Example 08 File Handling.txt';

 {We should check if the file exists first before trying to create it}
 ConsoleWindowWriteLn(WindowHandle,'Checking to see if ' + Filename + ' exists');
 if FileExists(Filename) then
  begin
   {If it does exist we can delete it}
   ConsoleWindowWriteLn(WindowHandle,'Deleting the file ' + Filename);
   DeleteFile(Filename);
  end;

 {Now create the file, let's use a TFileStream class to do this. We pass both the
  filename and the mode to TFileStream. fmCreate tells it to create a new file.}
 ConsoleWindowWriteLn(WindowHandle,'Creating a new file ' + Filename);
 {TFileStream will raise an exception if creating the file fails}
 try
  FileStream:=TFileStream.Create(Filename,fmCreate);

  {We've created the file, now we need to write some content to it, we can use
   a TStringList for that but there are many other ways as well.}
  StringList:=TStringList.Create;

  {Add some text to our string list}
  StringList.Add('Example 08 File Handling');
  StringList.Add('This is a test file created by the example');
  StringList.Add('Here is a another line of text as well.');

  {Since TStringList has a SaveToStream method, we can just call that to write
   all the strings to our new file.}
  ConsoleWindowWriteLn(WindowHandle,'Saving the TStringList to the file');
  StringList.SaveToStream(FileStream);

  {With that done we can close the file and free the string list}
  ConsoleWindowWriteLn(WindowHandle,'Closing the file');
  ConsoleWindowWriteLn(WindowHandle,'');
  FileStream.Free;
  StringList.Free;

  {Did it work? Let's open the file and display it on screen to see.}
  ConsoleWindowWriteLn(WindowHandle,'Opening the file ' + Filename);
  try
   FileStream:=TFileStream.Create(Filename,fmOpenReadWrite);

   {Recreate our string list}
   StringList:=TStringList.Create;

   {And use LoadFromStream to read it}
   ConsoleWindowWriteLn(WindowHandle,'Loading the TStringList from the file');
   StringList.LoadFromStream(FileStream);

   {Iterate the strings and print them to the screen}
   ConsoleWindowWriteLn(WindowHandle,'The contents of the file are:');
   for Count:=0 to StringList.Count - 1 do
    begin
     ConsoleWindowWriteLn(WindowHandle,StringList.Strings[Count]);
    end;

   {Close the file and free the string list again}
   ConsoleWindowWriteLn(WindowHandle,'Closing the file');
   ConsoleWindowWriteLn(WindowHandle,'');
   FileStream.Free;
   StringList.Free;

   {If you remove the SD card and put in back in your computer, you should see the
    file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
    see the contents exactly as they appeared on screen.}
  except
   {TFileStream couldn't open the file}
   ConsoleWindowWriteLn(WindowHandle,'Failed to open the file ' + Filename);
  end;
 except
  {Something went wrong creating the file}
  ConsoleWindowWriteLn(WindowHandle,'Failed to create the file ' + Filename);
 end;
 Filename:='C:\bb.bin';
 Filenamelog:='C:\read.log';
 ConsoleWindowWriteLn(WindowHandle,'starting to read '+Filename+'start of write '+Filenamelog);
 ConsoleWindowWriteLn(WindowHandle,'Opening the file ' + Filename+'Opening the file ' + Filenamelog);
  try
   FileStream:=TFileStream.Create(Filename,fmOpenReadWrite);
   //FileStreamlog:=TFileStream.Create(Filenamelog,fmOpenReadWrite);
   {Recreate our string list}
   StringList:=TStringList.Create;
   //StringListlog:=TStringList.Create;

   {And use LoadFromStream to read it}
   ConsoleWindowWriteLn(WindowHandle,'Loading the TStringList from the file');
   StringList.LoadFromStream(FileStream);
   ConsoleWindowWriteLn(WindowHandle,'Num of strings in file StringList.Count '+intToStr(StringList.Count));

   {PP is Pointer BP is a Pointer to an Buffer = array[0..40159] of Char; }
   PP:=StringList.GetText;
   BP:=PP;


   j:=Length(BP[0]);
   ConsoleWindowWriteLn(WindowHandle,'Length '+intToStr(j));
   ConsoleWindowWriteLn(WindowHandle,'This is all chars '+BP[0]);
   //StringListlog.add(BP[0]);
   i:=0;
   j:=0;
   while (i < 4160) do
     begin
       Characters:=Copy(BP[0],i,65);
       //CRC:=Copy(Characters,65,1);
       //ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+Characters+' '+CRC);
       ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+Characters);
       //StringList.add(intToStr(i)+' '+Characters);
       i:=i+65;
     end;
    {This prints the complete string}

   //ConsoleWindowWriteLn(WindowHandle,'');
   {Iterate the strings and print them to the screen}
   //ConsoleWindowWriteLn(WindowHandle,'Count '+ intToStr(Count)+ ' The contents of the file are:');

   {Close the file and free the string list again}
   ConsoleWindowWriteLn(WindowHandle,'Closing the file');
   //ConsoleWindowWriteLn(WindowHandle,'');
   FileStream.Free;
   StringList.Free;
   //FileStreamlog.Free;
   //StringListlog.Free;
   {If you remove the SD card and put in back in your computer, you should see the
    file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
    see the contents exactly as they appeared on screen.}
  except
   {TFileStream couldn't open the file}
   ConsoleWindowWriteLn(WindowHandle,'Failed to open the file ' + Filename);
  end;

 {Halt the thread}
 ThreadHalt(0);
end.

