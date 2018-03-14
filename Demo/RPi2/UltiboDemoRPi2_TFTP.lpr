program UltiboDemoRPi2_TFTP;

{Ultibo demo project for Raspberry Pi 2B                                       }
{                                                                              }
{For the Raspberry Pi A/B/A+/B+/Zero version see the file UltiboDemoRPi.lpr    }
{                                                                              }
{To compile this project select Build, Run from the Lazarus menu               }
{                                                                              }
{History: 1.0.0 - Initial release                                              }
{         1.0.1 - Add Raspberry Pi 3B support                                  }
{         1.0.2 - Add USB fixes for Raspberry Pi A/A+/Zero                     } 

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}

uses
 RaspberryPi2,
 GlobalConfig,
 GlobalConst,
 Platform,
 Threads,
 DemoMain,
 Logging,
 Console,
 ConsoleShell;
 
begin
 {Initialize the Demo}
 if InitDemo then
  begin
   {Run the Demo}
   RunDemo;
   
   {When we return start the Console shell}
   CONSOLE_SHELL_ENABLED:=True;
   CONSOLE_SHELL_POSITION:=CONSOLE_POSITION_TOPRIGHT;
   ConsoleShellDeviceAdd(ConsoleDeviceGetDefault,False);
   
   {And start the Console logging}
   CONSOLE_REGISTER_LOGGING:=True;
   CONSOLE_LOGGING_POSITION:=CONSOLE_POSITION_BOTTOMRIGHT;
   LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
   LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
   LoggingOutput('This is a log window, try connecting a USB device');
  end;
  
 {Halt the Main thread if it ever returns here}
 ThreadHalt(0); 
end.
