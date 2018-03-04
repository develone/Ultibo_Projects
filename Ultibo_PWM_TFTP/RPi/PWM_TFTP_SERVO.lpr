program PWM_TFTP_SERVO;

{$mode objfpc}{$H+}

{ Advanced example - PWM_TFTP                                             }
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
  RaspberryPi, {Include RaspberryPi2 to make sure all standard functions are included}
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  Console,
  Classes,
  WebStatus,


  uTFTP,
  Winsock2,
  { needed to use ultibo-tftp  }
  { needed for telnet }
      Shell,
     ShellFilesystem,
     ShellUpdate,
     RemoteShell,
  { needed for telnet }
  Logging,
  SysUtils,
  PWM;   {Include the PWM unit to allow access to the functions}

var
 MyPLoggingDevice : ^TLoggingDevice;
 PWM0Device:PPWMDevice;
 PWM1Device:PPWMDevice;
  Count:Integer;
 handle:TWindowHandle;
 Handle1:THandle;
 //HTTPListener:THTTPListener;
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

  ConsoleWindowWriteLn (Handle, s);

end;



procedure WaitForSDDrive;

begin

  while not DirectoryExists ('C:\') do sleep (500);

end;

begin
  {
 The following 3 lines are logging to the console}
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));


 {The following 2 lines are logging to a file
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));
 MyPLoggingDevice:=LoggingDeviceGetDefault;
 LoggingDeviceRedirectOutput(MyPLoggingDevice);}
 {Create a console window to show what is happening}
 handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);


 // wait for IP address and SD Card to be initialised.
 {WaitForSDDrive;
 IPAddress := WaitForIPComplete;}
 {Wait a few seconds for all initialization (like filesystem and network) to be done}
 Sleep(5000);


 {Display a startup message on the console}
 ConsoleWindowWriteLn(handle,'Starting PWM_TFTP example');
 ConsoleWindowWriteLn (handle, 'Local Address ' + IPAddress);
 SetOnMsg (@Msg);
 {Create and start the HTTP Listener for our web status page}
 //HTTPListener:=THTTPListener.Create;
 //HTTPListener.Active:=True;

 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 //WebStatusRegister(HTTPListener,'','',True);

 {First locate the PWM devices

  The Raspberry Pi has two PWM channels which will normally end up with the names
  PWM0 and PWM1 when the driver is included in an application.

  You could also use PWMDeviceFindByDescription() here and use the contants defined
  in the BCM2709 unit for BCM2709_PWM0_DESCRIPTION and BCM2709_PWM1_DESCRIPTION.

  Those values are for Raspberry Pi 2 so you would adjust to use BCM2708 or BCM2710 for
  the Raspberry Pi A/B/A+/B+/Zero or Raspberry Pi 3 respectively. The names of the
  constants also change to BCM2708 or BCM2710 for the different models as well}

 PWM0Device:=PWMDeviceFindByName('PWM0');

 if (PWM0Device <> nil)   then
  begin
   {This example uses the default GPIO pin values which are GPIO_PIN_18 for PWM0
    and GPIO_PIN_19 for PWM1. If you need to use one of the alternate GPIO pins
    then you can call PWMDeviceSetGPIO() with the required pin number.

   You can also use PWMDeviceGetGPIO() to find out the currently configured pin}
   PWMDeviceSetGPIO(PWM0Device, GPIO_PIN_18);

   ConsoleWindowWriteLn(Handle,'PWM 0 pin set to gpio pin ' + inttostr(PWMDeviceGetGPIO(PWM0Device)));

   {On the Raspberry Pi the PWM setup requires 3 values.

    The first is the Mode which can be PWM_MODE_MARKSPACE, PWM_MODE_BALANCED or
    PWM_MODE_SERIALIZED. These are described in detail in the BCM2835 ARM Peripherals
    documentation which can be found via the resources page on the Ultibo wiki.

    The second value is the Frequency which controls the frequency of the clock
    used by the PWM device. On the Raspberry Pi both PWM devices share a common
    clock so changing the frequency on one device also changes it on the other.

    The final setup value is the Range, the exact meaning of the range value varies
    depending on the mode selected but in general it represents the time period of
    one full cycle of the waveform output by the device.

    The range and the data define what is actually output onto the GPIO pin, as an
    alternative to setting them individually you can call PWMDeviceConfigure() which
    allows you to specify both a Range and a Duty cycle in nanoseconds.

    Try experimenting with the range and data values to see how they affect the LEDs}

   {Setup PWM device 0}
   {Set the range to 2000}
   PWMDeviceSetRange(PWM0Device,2000);
   {And the mode to PWM_MODE_MARKSPACE}
   PWMDeviceSetMode(PWM0Device,PWM_MODE_MARKSPACE);
   {Finally set the frequency to 50KHz
   19200000/192/2000
   https://learn.adafruit.com/adafruits-raspberry-pi-lesson-8-using-a-servo-motor/software}
   PWMDeviceSetFrequency(PWM0Device,100000);
   ConsoleWindowWriteLn(Handle,'PWM 0 Freq ' + inttostr(PWMDeviceGetFrequency(PWM0Device)));
   ConsoleWindowWriteLn(Handle,'PWM 0 Range ' + inttostr(PWMDeviceGetRange(PWM0Device)));
   {Start the PWM devices

    This will start the clock and enable the devices, the final step to
    output something is to write some actual data which will specify how
    many pulses are output within the time period defined by the range.

    A data value of 0 will turn off the output whereas a data value equal
    to the range will mean the pulses are continuous. We can use this to
    make our LED go from fully off to fully on in gradual steps, the time
    it takes to make this transition is simply controlled by the value
    passed to Sleep()}
   if (PWMDeviceStart(PWM0Device) = ERROR_SUCCESS)  then
    begin

     {Start an endless loop writing data values to the PWM devices}
     {start of While}
     while True do
      begin
       {Cycle the devices through the entire range from 0 to 1023.

        The PWM0 device goes upwards (from off to full brightness)
        and the PWM1 device goes down (from full brightness to off)}

       {Count :=100;
       PWMDeviceWrite(PWM0Device,Count);
       ConsoleWindowWriteLn(Handle,'PWM 0 ' + inttostr(Count));
       Sleep(100);

       Count :=150;
       PWMDeviceWrite(PWM0Device,Count);
       ConsoleWindowWriteLn(Handle,'PWM 0 ' + inttostr(Count));
       Sleep(100);

       Count :=200;
       PWMDeviceWrite(PWM0Device,Count);
       ConsoleWindowWriteLn(Handle,'PWM 0 ' + inttostr(Count));
       Sleep(100);

       Count :=250;
       PWMDeviceWrite(PWM0Device,Count);
       ConsoleWindowWriteLn(Handle,'PWM 0 ' + inttostr(Count));
       Sleep(100);}

       {start of for loop}
       for Count:=50 to 250 do
        begin
         PWMDeviceWrite(PWM0Device,Count);
         //ConsoleWindowWriteLn(Handle,'PWM 0 ' + inttostr(Count));

         Sleep(10);
        end;
       {end of for loop}
        ConsoleWindowWriteLn(Handle,'End of up loop');

       {start of for loop}
       for Count:=250 downto 50 do
        begin
         PWMDeviceWrite(PWM0Device,Count);
         //ConsoleWindowWriteLn(Handle,'PWM 0 ' + inttostr(Count));
         Sleep(10);
        end;
       {end of for loop}
        ConsoleWindowWriteLn(Handle,'End of dn loop');
        Sleep(20);
        ConsoleWindowWriteLn(Handle,'End of While');
      end;
      {end of While}

     {Stop the PWM devices

      This will disable the devices and stop the clock, remember that the
      clock is shared between both devices so the driver will only actually
      stop the clock when PWMDeviceStop() is called for both of them}
     PWMDeviceStop(PWM0Device);

    end
   else
    begin
     ConsoleWindowWriteLn(Handle,'Error: Failed to start PWM devices 0 and 1');
    end;
  end
 else
  begin
   ConsoleWindowWriteLn(Handle,'Error: Failed to locate PWM devices 0 and 1');
  end;

 {Halt this thread}
 ThreadHalt(0);
end.


