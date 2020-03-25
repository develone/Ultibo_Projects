unit uMySlip;

{$mode objfpc}{$H+}

interface

uses
  {RaspberryPi2,}
  GlobalConst,
  Threads,
  Classes,
  SysUtils,
  Console,
  Network,
  Platform,
  Serial;

var
 Count:LongWord;

 Characters:String;

 MySlip1_ResultOpenStatus:LongWord;
 MySlip1_ResultCloseStatus:LongWord;
 ResultTransmit:LongWord;

 procedure MySlip (PassHandle2:THandle );
 function MySlipOpen( PassHandle2:THandle) : LongWord;
 function MySlipClose( PassHandle2:THandle) : LongWord;
 function MySlipTransmit( PassHandle2:THandle; PassBuffer:Pointer; PassCount:LongWord) : LongWord;

implementation

 procedure MySlip (PassHandle2:THandle );
  begin
     ConsoleWindowWriteLn (PassHandle2, 'uMySlip');
  end;

 function MySlipOpen( PassHandle2:THandle ) : LongWord;
   begin
      {Setting MySlip1_ResultOpenStatus to 0 can not SerialOpen}
      MySlip1_ResultOpenStatus:=0;
      if SerialOpen(1000000,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0) = ERROR_SUCCESS then
        begin
           {Opened successfully, display a message}
            ConsoleWindowWriteLn (PassHandle2, 'Serial device opened ' + 'My_Slip1ResultOpenStatus '+ intToStr(MySlip1_ResultOpenStatus));
            MySlip1_ResultOpenStatus:=1;
            Result := MySlip1_ResultOpenStatus;
        end;

   end;
 function MySlipClose( PassHandle2:THandle) : LongWord;
   var
    lclcstatus: LongWord;
   begin
      {Close the serial device using SerialClose}
      lclcstatus:=SerialClose;
      ConsoleWindowWriteLn(PassHandle2,'Serial device closed ' + intToStr(lclcstatus));
      MySlip1_ResultCloseStatus:=lclcstatus;
      Result := MySlip1_ResultCloseStatus;


   end;
 function MySlipTransmit( PassHandle2:THandle; PassBuffer:Pointer; PassCount:LongWord) : LongWord;
   type
     Buffer = String[255];
     BufPtr = ^Buffer;
   var

    BP:BufPtr;
    Characters:String;
   begin

      BP := PassBuffer;
      Characters:=BP^;
      ConsoleWindowWriteLn(PassHandle2, Characters);
      ConsoleWindowWriteLn(PassHandle2,' number to write ' + intToStr(PassCount));

      SerialWrite(PChar(Characters),Passcount,PassCount);

      //ConsoleWindowWriteLn(PassHandle2,' number wrote ' + intToStr(PassCount));
      ConsoleWindowWriteLn(PassHandle2,'');
      Result := PassCount;
   end;

end.

