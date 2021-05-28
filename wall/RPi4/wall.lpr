program wall;

{$mode objfpc}{$H+}

{ Advanced example - Template                                             }
{                                                                         }
{ This example shows how to create  webserver and TFTP server with remote shell  }
{ This version is for Raspberry Pi 2B and will also work on a 3B.                }

{ After the first time that kernel7.img has been transferred to micro sd card    }
{ tftp xx.xx.xx.xx < cmdstftp                                                    }
{ contents of cmdstftp                                                           }
{ binary                                                                         }
{ put kernel7.img                                                                }
{ quit                                                                           }

uses
  {InitUnit,     Include InitUnit to allow us to change the startup behaviour}
  RaspberryPi4, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Classes,
  GraphicsConsole, {Include the GraphicsConsole unit so we can create a graphics window}
  BMPcomn,         {Include the BMPcomn unit from the fpc-image package to give the Bitmap headers}
  uBitmap,
  Console,
  HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
  WebStatus,
  SysUtils,  { TimeToStr & Time }
  Logging,
  uTFTP,
  Winsock2,
  { needed to use ultibo-tftp  }
  { needed for telnet }
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell;
  { needed for telnet }

var
 Window:TWindowHandle;
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

   ConsoleWindowWriteLn (Window, s);

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

 {The following 2 lines are logging to a file }
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\imagelogging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));
 {Create a graphics window to display our bitmap, let's use the new CONSOLE_POSITION_FULLSCREEN option }
 Window:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL);


 {Create a console window to show what is happening}
 //Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);

 {Display a startup message on the console}
 ConsoleWindowWriteLn(Window,'Starting TFTP_Template example');
  // wait for IP address and SD Card to be initialised.
 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 ConsoleWindowWriteLn (Window, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);
 {Loop endlessly}
  while True do
    begin

         DrawBitmap(Window,'C:\image1.bmp',0,0);
         DrawBitmap(Window,'C:\image2.bmp',192,0);
         DrawBitmap(Window,'C:\image3.bmp',832,0);
         Sleep(4000);
         DrawBitmap(Window,'C:\image3.bmp',0,500);
         DrawBitmap(Window,'C:\image2.bmp',500,500);
         DrawBitmap(Window,'C:\image1.bmp',1140,500);
         Sleep(2000);
         DrawBitmap(Window,'C:\image3.bmp',0,0);
         DrawBitmap(Window,'C:\image2.bmp',500,0);
         DrawBitmap(Window,'C:\image1.bmp',1140,0);
         Sleep(2000);
         {
         DrawBitmap(Window,'C:\image3.bmp',0,256);
         DrawBitmap(Window,'C:\image3.bmp',256,256);
         DrawBitmap(Window,'C:\image1.bmp',512,256);

         DrawBitmap(Window,'C:\image1.bmp',768,256);
         DrawBitmap(Window,'C:\image2.bmp',1024,256);
         DrawBitmap(Window,'C:\image3.bmp',1280,256);
         DrawBitmap(Window,'C:\image4.bmp',0,512);
         {Sleep for 8 second}
         Sleep(8000);

         DrawBitmap(Window,'C:\image3.bmp',0,0);
         DrawBitmap(Window,'C:\image2.bmp',256,0);
         DrawBitmap(Window,'C:\image1.bmp',512,0);

         DrawBitmap(Window,'C:\image1.bmp',768,0);
         DrawBitmap(Window,'C:\image3.bmp',1024,0);
         DrawBitmap(Window,'C:\image2.bmp',1280,0);



         DrawBitmap(Window,'C:\image2.bmp',0,256);
         DrawBitmap(Window,'C:\image3.bmp',256,256);
         DrawBitmap(Window,'C:\image1.bmp',512,256);

         DrawBitmap(Window,'C:\image2.bmp',768,256);
         DrawBitmap(Window,'C:\image3.bmp',1024,256);
         DrawBitmap(Window,'C:\image1.bmp',1280,256);
         }
         {Sleep for 8 second}
         {Sleep(8000)}

  end;

 {Halt this thread}
 ThreadHalt(0);
end.




