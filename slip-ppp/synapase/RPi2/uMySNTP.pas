unit uMySNTP;

{$mode objfpc}{$H+}

interface
uses
  GlobalConst,
  Threads,
  Classes,
  SysUtils,
  Console,
  SNTPsend;
  { needed for SNTP
  SNTPsend;
   needed for SNTP } 

   var
   SntpClient:TSntpSend; 

 procedure MySNTP (Passhandle2:THandle );

implementation

 procedure MySNTP (Passhandle2: THandle);
  begin
{Create the SNTP client}

 PassHandle2:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMRIGHT,True);
 ConsoleWindowWriteLn(PassHandle2,'Starting Synapse STNP Example');
 SntpClient:=TSntpSend.Create;
 try
  SntpClient.TargetHost:='pool.ntp.org';
  ConsoleWindowWriteLn(PassHandle2,'Requesting SNTP time from ' + SntpClient.TargetHost);
  ConsoleWindowWriteLn(PassHandle2,'');

  if SntpClient.GetSNTP then
   begin
    ConsoleWindowWriteLn(Passhandle2,'SNTP time is ' + DateTimeToStr(SntpClient.NTPTime) + ' UTC');
   end
  else
   begin
    ConsoleWindowWriteLn(PassHandle2,'Could not contact SNTP server ' + SntpClient.TargetHost);
   end;
 finally
  SntpClient.Free;
 end;
 end;
 end.
