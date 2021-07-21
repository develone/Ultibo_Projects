program SerialConnection;

{$mode objfpc}{$H+}

{ Example 13 Serial Connection                                                 }
{                                                                              }
{ This example uses the serial (UART) device to send output and receive input. }
{ Anything received as input will be echoed back as output.                    }
{                                                                              }
{ QEMU has a built in serial console (select View, serial0 from the menu) so   }
{ you can try this example without any extra equipement. Simply type into the  }
{ serial console and when you press enter the message will be echoed back.     }
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
  Framebuffer,
  SysUtils,
  Serial;   {Include the Serial unit so we can open, read and write to the device}

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
 ConsoleWindowWriteLn(WindowHandle,'Welcome to Example 13 Serial Connection');
 ConsoleWindowWriteLn(WindowHandle,'Go to the serial console by selecting View, serial0 from the menu');

 {First we need to open the serial device and set the speed and other parameters.

  Because this is the QEMU version of the example the settings are not really used
  because the emulated hardware doesn't do anything different no matter what settings
  we choose. It's still a good idea to set them anyway because you'll need to do it
  when running on real hardware.

  We can use the SerialOpen function in the Platform unit to open the default serial
  device or we can use the SerialDeviceOpen function in the Serial unit if we need
  to specify which device to open.

  We'll use SerialOpen and specify 9600 as the speed with 8 data bits, 1 stop bit,
  no parity and no flow control. The constants used here can be found in the GlobalConst
  unit.

  The last 2 parameters allow setting the size of the transmit and receive buffers,
  passing 0 means use the default size.}
 if SerialOpen(9600,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0) = ERROR_SUCCESS then
  begin

   {Opened successfully, display a message}
   ConsoleWindowWriteLn(WindowHandle,'Serial device opened, type some text in the serial console and press Enter');

   {Setup our starting point}
   Count:=0;
   Characters:='';

   {Loop endlessly waiting for data}
   while True do
    begin
     {Read from the serial device using the SerialRead function, to be safe we
      would normally check the result of this function before using the value}
     SerialRead(@Character,SizeOf(Character),Count);

     {Check what character we received}
     if Character = #13 then
      begin
       {If we received a carriage return then write our characters to the console}
       ConsoleWindowWriteLn(WindowHandle,'Received a line: ' + Characters);

       {Check for the word Quit}
       if Uppercase(Characters) = 'QUIT' then
        begin
         {If received then say goodbye and exit our loop}
         Characters:='Goodbye!' + Chr(13) + Chr(10);
         SerialWrite(PChar(Characters),Length(Characters),Count);

         {Wait for the data to be sent}
         Sleep(1000);

         Break;
        end;

       {Add a carriage return and line feed}
       Characters:=Characters + Chr(13) + Chr(10);

       {And echo them back to the serial device using SerialWrite}
       SerialWrite(PChar(Characters),Length(Characters),Count);

       {Now clear the characters and wait for more}
       Characters:='';
      end
     else
      begin
       {Add the character to what we have already recevied}
       Characters:=Characters + Character;
      end;

     {No need to sleep on each loop, SerialRead will wait until data is received}
    end;

   {Close the serial device using SerialClose}
   SerialClose;

   ConsoleWindowWriteLn(WindowHandle,'Serial device closed');
  end
 else
  begin
   {Must have been an error, print a message on the console}
   ConsoleWindowWriteLn(WindowHandle,'An error occurred opening the serial device');
  end;

 {Halt the thread if we exit the loop}
 ThreadHalt(0);
end.


