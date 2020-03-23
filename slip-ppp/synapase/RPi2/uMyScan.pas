unit uMyScan;


{$mode objfpc}{$H+}

interface
uses
  GlobalConst,
  Threads,
  Classes,
  SysUtils,
  Console,
  { needed for scan }
  IPUtils,
  PingThread
  { needed for scan }  ;

 var
  i,j:Cardinal;
  PingStart:Cardinal;
  PingEnd:Cardinal;
  PingCount:Cardinal;
  PingResults:array of TPingResult;
  PingThreads:array of TPingThread;
  ThreadsComplete:Boolean;

 procedure MyScan (Passhandle1: THandle; PassScanStart:String; PassScanEnd:String);

implementation

procedure MyScan (Passhandle1: THandle; PassScanStart:String; PassScanEnd:String);
begin
 {Check start and end}
 if IsIPAddress(PassScanStart) and IsIPAddress(PassScanEnd) then
  begin
   PingStart:=IPToCardinal(StrToIP(PassScanStart));
   PingEnd:=IPToCardinal(StrToIP(PassScanEnd));

   //Display addresses
   ConsoleWindowWriteLn(PassHandle1,'Start address  ' + PassScanStart);
   ConsoleWindowWriteLn(PassHandle1,'End address  ' + PassScanEnd);
   ConsoleWindowWriteLn(PassHandle1,'');

   //Get count
   PingCount:=(PingEnd - PingStart) + 1;

   //Display count
   ConsoleWindowWrite(PassHandle1,'Pinging ' + IntToStr(PingCount) + ' addresses');

   //Initialize arrays
   SetLength(PingResults,PingCount);
   SetLength(PingThreads,PingCount);
   j:=0;
   for i:=PingStart to PingEnd do
    begin
     PingResults[j].IPAddress:=IPToStr(CardinalToIP(i));
     PingResults[j].Exists:=False;
     Inc(j);
    end;


      //Create one thread for each ping
   for i:=0 to PingCount - 1 do
    begin
     PingThreads[i]:=TPingThread.Create(PingResults[i]);
    end;

   ConsoleWindowWrite(PassHandle1,' ');

   //Wait till all threads are executed
   repeat
    ThreadsComplete:=True;
    ConsoleWindowWrite(PassHandle1,'.');
    Sleep(1000);
    for i:=0 to PingCount - 1 do
     begin
      if not PingThreads[i].Ready then
       begin
        ThreadsComplete:=False;
        Break;
       end;
     end;
   until ThreadsComplete;

   ConsoleWindowWriteLn(PassHandle1,'');
   ConsoleWindowWriteLn(PassHandle1,'');
   ConsoleWindowWriteLn(PassHandle1,'Ping Results');

   //Dislay results
   for i:=0 to PingCount - 1 do
    begin
     if PingThreads[i].PingResult.Exists then
      begin
       ConsoleWindowWriteLn(PassHandle1,IntToStr(i + 1) + '  ' + PingThreads[i].PingResult.IPAddress);
      end;
    end;

 
      //Free threads
   for i:=0 to PingCount - 1 do
    begin
     PingThreads[i].Free;
    end;
   end;



end;
 {
else
  begin
   ConsoleWindowWriteLn(PassHandle1,'Invalid start or end address for scan');
  end;
 }
end.
