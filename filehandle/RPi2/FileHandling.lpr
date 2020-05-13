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
{  Once compiled copy the kernel7.img file to an SD card along with the        }
{  firmware files and use it to boot your Raspberry Pi.                        }
{                                                                              }
{  Raspberry Pi 2B version                                                     }
{   What's the difference? See Project, Project Options, Config and Target.    }

{ After the first time that kernel7.img has been transferred to micro sd card    }
{ tftp xx.xx.xx.xx < cmdstftp                                                    }
{ contents of cmdstftp                                                           }
{ binary                                                                         }
{ put kernel7.img                                                                }
{ quit                                                                           }

uses
  {InitUnit,     Include InitUnit to allow us to change the startup behaviour}
  RaspberryPi2, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Classes,     {Include the common classes}
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  Framebuffer,
  BCM2836,
  SysUtils,  { TimeToStr & Time }
  Logging,
  uTFTP,
  Winsock2,
  FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC,         {Include the MMC/SD core to access our SD card}
  BCM2709,     {And also include the MMC/SD driver for the Raspberry Pi}
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell;
  { needed for telnet }

var
 Count:Integer;
 Filename:String;
 SearchRec:TSearchRec;
 StringList:TStringList;
 FileStream:TFileStream;
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



begin
 {The following 3 lines are logging to the console
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
 }

 {The following 2 lines are logging to a file
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE)); }


 {Create a console window to show what is happening}
 LeftWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);

 {Display a startup message on the console}
 ConsoleWindowWriteLn(LeftWindow,'Starting TFTP_Template example');
  // wait for IP address and SD Card to be initialised.
 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 ConsoleWindowWriteLn (LeftWindow, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);

 {To list the contents we need to use FindFirst/FindNext, start with FindFirst}
  if FindFirst('C:\*.*',faAnyFile,SearchRec) = 0 then
   begin
    {If FindFirst succeeds it will return 0 and we can proceed with the search}
    repeat
     {Print the file found to the screen}
     ConsoleWindowWriteLn(LeftWindow,'Filename is ' + SearchRec.Name + ' - Size is ' + IntToStr(SearchRec.Size) + ' - Time is ' + DateTimeToStr(FileDateToDateTime(SearchRec.Time)));

    {We keep calling FindNext until there are no more files to find}
    until FindNext(SearchRec) <> 0;
   end;

  {After any call to FindFirst, you must call FindClose or else memory will be leaked}
  FindClose(SearchRec);
  ConsoleWindowWriteLn(LeftWindow,'');

  {Let's try creating a file and writing some text to it, we'll assign our filename
   to a variable.}
  Filename:='C:\test0513.txt';

  {We should check if the file exists first before trying to create it}
  ConsoleWindowWriteLn(LeftWindow,'Checking to see if ' + Filename + ' exists');
  if FileExists(Filename) then
   begin
    {If it does exist we can delete it}
    ConsoleWindowWriteLn(LeftWindow,'Deleting the file ' + Filename);
    DeleteFile(Filename);
   end;

  {Now create the file, let's use a TFileStream class to do this. We pass both the
   filename and the mode to TFileStream. fmCreate tells it to create a new file.}
  ConsoleWindowWriteLn(LeftWindow,'Creating a new file ' + Filename);
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
   ConsoleWindowWriteLn(LeftWindow,'Saving the TStringList to the file');
   StringList.SaveToStream(FileStream);

   {With that done we can close the file and free the string list}
   ConsoleWindowWriteLn(LeftWindow,'Closing the file');
   ConsoleWindowWriteLn(LeftWindow,'');
   FileStream.Free;
   StringList.Free;

   {Did it work? Let's open the file and display it on screen to see.}
   ConsoleWindowWriteLn(LeftWindow,'Opening the file ' + Filename);
   try
    FileStream:=TFileStream.Create(Filename,fmOpenReadWrite);

    {Recreate our string list}
    StringList:=TStringList.Create;

    {And use LoadFromStream to read it}
    ConsoleWindowWriteLn(LeftWindow,'Loading the TStringList from the file');
    StringList.LoadFromStream(FileStream);

    {Iterate the strings and print them to the screen}
    ConsoleWindowWriteLn(LeftWindow,'The contents of the file are:');
    for Count:=0 to StringList.Count - 1 do
     begin
      ConsoleWindowWriteLn(LeftWindow,StringList.Strings[Count]);
     end;

    {Close the file and free the string list again}
    ConsoleWindowWriteLn(LeftWindow,'Closing the file');
    ConsoleWindowWriteLn(LeftWindow,'');
    FileStream.Free;
    StringList.Free;

    {If you remove the SD card and put in back in your computer, you should see the
     file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
     see the contents exactly as they appeared on screen.}
   except
    {TFileStream couldn't open the file}
    ConsoleWindowWriteLn(LeftWindow,'Failed to open the file ' + Filename);
   end;
  except
   {Something went wrong creating the file}
   ConsoleWindowWriteLn(LeftWindow,'Failed to create the file ' + Filename);
  end;

 {Halt this thread}
 ThreadHalt(0);
end.




