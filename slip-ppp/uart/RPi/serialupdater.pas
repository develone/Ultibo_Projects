unit serialupdater;
{unit receives YMODEM transfers}
{only tested with ExtraPUTTY}
{any file sent will be saved to SD card}
{if the file is called kernel.img then the unit will save it and then restart the Pi}
{tested with a HC-05 bluetooth module attached to the GPIO header on a Pi Zero W}

{$mode objfpc}{$H+}

{ comment this out if you want the updater hidden }
{$DEFINE output2screen}



interface

uses
  Classes, SysUtils, Threads,
  {$IFDEF output2screen}
  Console,       {needed for ConsoleWindowCreate}
  GlobalTypes,   {needed for TWindowHandle}
  {$ENDIF}
  GlobalConst,   {needed for many things}
  GlobalConfig,  {needed for THREAD_STACK_DEFAULT_SIZE}
  Platform,      {needed for systemrestart}
  FileSystem,    {Include the file system core and interfaces}
  FATFS,         {Include the FAT file system driver}
  MMC,           {Include the MMC/SD core to access our SD card}
  BCM2708,       {And also include the MMC/SD driver for the Raspberry Pi}
  { uncomment if you want to use a USB to serial adapter }
  { and change the serialdevice in the code }
  {FTDISerial, RaspberryPi,}
  Serial
  ;

const
  {$IFDEF output2screen}
  CONSOLE_POSITION = CONSOLE_POSITION_RIGHT;
  {$ENDIF}
  BAUD_115200 = 230400;
  DATA_BITS_8 = 8;
  STOP_BITS_1 = 1;
  PARITY_NONE = 0;
  FLOW_RTS_CTS = 1;
  SOH = 1;
  STX = 2;
  EOT = 4;
  ACK = 6;
  NAK = $15;
  ETB = $17;
  CAN = $18;
  C = $43;
  CRC_POLY_CCITT = $1021;
  crc_tabccitt_init:boolean = false;
  crc_tabccitt:array[0..255] of word = (
  $0000, $1021, $2042, $3063, $4084, $50A5, $60C6, $70E7,
  $8108, $9129, $A14A, $B16B, $C18C, $D1AD, $E1CE, $F1EF,
  $1231, $0210, $3273, $2252, $52B5, $4294, $72F7, $62D6,
  $9339, $8318, $B37B, $A35A, $D3BD, $C39C, $F3FF, $E3DE,
  $2462, $3443, $0420, $1401, $64E6, $74C7, $44A4, $5485,
  $A56A, $B54B, $8528, $9509, $E5EE, $F5CF, $C5AC, $D58D,
  $3653, $2672, $1611, $0630, $76D7, $66F6, $5695, $46B4,
  $B75B, $A77A, $9719, $8738, $F7DF, $E7FE, $D79D, $C7BC,
  $48C4, $58E5, $6886, $78A7, $0840, $1861, $2802, $3823,
  $C9CC, $D9ED, $E98E, $F9AF, $8948, $9969, $A90A, $B92B,
  $5AF5, $4AD4, $7AB7, $6A96, $1A71, $0A50, $3A33, $2A12,
  $DBFD, $CBDC, $FBBF, $EB9E, $9B79, $8B58, $BB3B, $AB1A,
  $6CA6, $7C87, $4CE4, $5CC5, $2C22, $3C03, $0C60, $1C41,
  $EDAE, $FD8F, $CDEC, $DDCD, $AD2A, $BD0B, $8D68, $9D49,
  $7E97, $6EB6, $5ED5, $4EF4, $3E13, $2E32, $1E51, $0E70,
  $FF9F, $EFBE, $DFDD, $CFFC, $BF1B, $AF3A, $9F59, $8F78,
  $9188, $81A9, $B1CA, $A1EB, $D10C, $C12D, $F14E, $E16F,
  $1080, $00A1, $30C2, $20E3, $5004, $4025, $7046, $6067,
  $83B9, $9398, $A3FB, $B3DA, $C33D, $D31C, $E37F, $F35E,
  $02B1, $1290, $22F3, $32D2, $4235, $5214, $6277, $7256,
  $B5EA, $A5CB, $95A8, $8589, $F56E, $E54F, $D52C, $C50D,
  $34E2, $24C3, $14A0, $0481, $7466, $6447, $5424, $4405,
  $A7DB, $B7FA, $8799, $97B8, $E75F, $F77E, $C71D, $D73C,
  $26D3, $36F2, $0691, $16B0, $6657, $7676, $4615, $5634,
  $D94C, $C96D, $F90E, $E92F, $99C8, $89E9, $B98A, $A9AB,
  $5844, $4865, $7806, $6827, $18C0, $08E1, $3882, $28A3,
  $CB7D, $DB5C, $EB3F, $FB1E, $8BF9, $9BD8, $ABBB, $BB9A,
  $4A75, $5A54, $6A37, $7A16, $0AF1, $1AD0, $2AB3, $3A92,
  $FD2E, $ED0F, $DD6C, $CD4D, $BDAA, $AD8B, $9DE8, $8DC9,
  $7C26, $6C07, $5C64, $4C45, $3CA2, $2C83, $1CE0, $0CC1,
  $EF1F, $FF3E, $CF5D, $DF7C, $AF9B, $BFBA, $8FD9, $9FF8,
  $6E17, $7E36, $4E55, $5E74, $2E93, $3EB2, $0ED1, $1EF0);


type
   TSerialUpdaterThread = class(TThread)
   private
     filename: shortstring;
     SerialDevice: PSerialDevice;
     {$IFDEF output2screen}
     WindowHandle:TWindowHandle;
     {$ENDIF}
     procedure WaitForFile;
     function VerifyPacket(PPacket: PByte; out APacketNumber: LongWord; out ADataPosition: LongWord; out ADataLength: LongWord): boolean;
     function calcCRC(const input_str:PByte; num_bytes:integer): word;inline;
   public
     procedure Execute; override;
   end;

   TBuffer = Packed Array [1..1500] of byte;

   TFileState = (
     FS_Waiting,
     FS_FileOpen,
     FS_FileFinished,
     FS_FileCopied
     );

implementation

var
  updater: TSerialUpdaterThread;

procedure TSerialUpdaterThread.Execute;
var
  success: boolean=false;
begin
  {$IFDEF output2screen}
  WindowHandle:= ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION, true);
  ConsoleWindowWriteLn (WindowHandle, 'Updater thread');
  {$ENDIF}

  filename:='';
  SerialDevice:=nil;
  while SerialDevice=nil do
  begin
    { Specify other serial devices here }
    { for example for an FTDI cable }
    { SerialDevice:=SerialDeviceFindByDescription ('FTDI USB to Serial'); }
    { but you must add FTDISerial & RaspberryPi to uses }
    { default is GPIO header }
    { Tx is GPIO14 / header pin 8 }
    { Rx is GPIO15 / header pin 10 }
    { remember it's 3.3V :-) }
    SerialDevice:=SerialDeviceGetDefault;
    if SerialDevice=nil then sleep(200);
  end;

  repeat
    success:= (SerialDeviceOpen (SerialDevice, BAUD_115200, DATA_BITS_8, STOP_BITS_1, PARITY_NONE, FLOW_RTS_CTS, 0, 0)
     = ERROR_SUCCESS);
    if NOT success then
               begin
                 {$IFDEF output2screen} ConsoleWindowWriteLn (WindowHandle, 'Serial fail');  {$ENDIF}
                 sleep(1000);
               end
               else
               begin
                 {$IFDEF output2screen} ConsoleWindowWriteLn (WindowHandle, 'Serial open');  {$ENDIF}
               end;
  until success;
  repeat
    WaitForFile;
    {$IFDEF output2screen} ConsoleWindowWriteLn (WindowHandle, 'Execute loop, filename='+filename); {$ENDIF}
  until filename='C:\kernel.img';
  sleep(4000);
  SystemRestart(0);
end;

procedure TSerialUpdaterThread.WaitForFile;
var
  StartTime: qword=0;
  EndTime: qword=0;
  TimeLimit: qword=0;
  Buffer: TBuffer;
  PacketLength: word=0;
  MinLength: word=0;
  {$IFDEF output2screen} Tempstr: ansistring; {$ENDIF}
  count: longword=0;
  PacketNumber, DataPosition, DataLength: longword;
  LastPacketNumber: longword=0;
  PacketFound: boolean=false;
  StringofNumbers: shortstring;
  FileLength: longword=0;
  LengthWritten: longword=0;
  EndFlag: boolean=false;
  FileNameNew: shortstring;
  FileStream:TFileStream;
  BlockInMemory: TMemoryStream;
  FileState: TFileState;
  Done: boolean=false;
begin
  TimeLimit:=3000;
  Done:=false;
  while not Done do
  begin
    {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'Reset'); {$ENDIF}
    FileState:=FS_Waiting;
    LengthWritten:=0;
    FileLength:=0;
    Count:=0;
    Filename:='';
    { C instructs clients to send 1024 length data, NAK should cause 128 - 128 is untested }
    Buffer[1]:=C;
    PacketLength:=1;
    SerialDeviceWrite (SerialDevice, @Buffer[1],PacketLength, SERIAL_WRITE_NONE, Count);
    StartTime:=GetTickCount64;
    PacketLength:=0;
    MinLength:=1029; //133 for 128 size blocks
    PacketFound:=false;
    repeat //until ((EndTime-StartTime)>TimeLimit)  OR Done;
      { see what client has sent to us }
      { keep adding it the buffer until it means something }
      SerialDeviceRead (SerialDevice, @Buffer[PacketLength+1], Length(buffer)-PacketLength, SERIAL_READ_NON_BLOCK, Count);
      if Count=0 then sleep(1)
                 else
                 begin
                   PacketLength:=PacketLength+Count;
                   StartTime:=GetTickCount64;
                   TimeLimit:=8000;
                 end;
      { see if the first three bytes of a packet are there }
      if (NOT PacketFound) AND (PacketLength>=3) then
      begin
        Count:=1;
        repeat
          if     ((Buffer[Count]=SOH) OR (Buffer[Count]=STX))
             AND ((Buffer[Count+1]+Buffer[Count+2])=$FF)
             then PacketFound:=true
             else inc(Count);
        until ((Count+2)>PacketLength) OR PacketFound;
        if PacketFound then
          case Buffer[Count] of
            SOH: MinLength:=132+Count;
            STX: Minlength:=1028+Count;
          end;
      end;

      { maybe you want to use your serial port for other stuff as well }
      if (NOT PacketFound) AND (PacketLength>0) then
      begin
        { Add other code here for stuff that's not from a YMODEM client}

      end;

      { if the file is finished wait for EOT and save the file }
      if (FileState=FS_FileFinished) OR (FileState=FS_FileCopied) then
      begin
        if PacketLength>0 then
        begin
          if Buffer[PacketLength]=EOT then
          begin
            {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'got EOT'); {$ENDIF}
            PacketLength:=0;
            Buffer[1]:=ACK;
            SerialDeviceWrite (SerialDevice, @Buffer[1], 1, SERIAL_WRITE_NONE, Count);
            StartTime:=GetTickCount64;
            if FileState=FS_FileFinished then
            begin
              Filename:='C:\'+Filename;
              { is the file already on the SD card }
              if FileExists(Filename) then
              begin
                {If it does exist we can delete it}
                {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'Deleting the file ' + Filename);   {$ENDIF}
                DeleteFile(Filename);
              end;
              {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'renaming ' + Filenamenew + ' to ' + FileName); {$ENDIF}
              { save the file that the client sent }
              if RenameFile(FileNameNew,FileName) then
              begin
                {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'Done'); {$ENDIF}
              end
              else
              begin
                {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'oh dear' + Filename + ' lost!');       {$ENDIF}
                { just in case kernel.img got lost }
                { at least you can try again }
                FileName:=''; //stops reboot
              end;
              FileState:=FS_FileCopied;
            end;
          end;
        end;
      end;

      { We might have a packet and it's long enough }
      if PacketFound AND (PacketLength>=MinLength) then
      begin
        if VerifyPacket(@Buffer[1], PacketNumber, DataPosition, DataLength) then
        begin
          case FileState of
            FS_Waiting:
              { extract the filename from packet zero and open the file on the SD card }
              if PacketNumber=0 then
              begin
                 filename:='';
                 FileLength:=0;
                 EndFlag:=false;
                 count:=DataPosition+1;
                 repeat
                   { only 0 to 9, A to Z, a to z & . supported }
                   { add more ascii chars if you want fancy filenames }
                   case buffer[count] of
                     46      : filename:=filename+chr(buffer[count]); //.
                     48..57  : filename:=filename+chr(buffer[count]); //0 to 9
                     65..90  : filename:=filename+chr(buffer[count]); //A to Z
                     97..122 : filename:=filename+chr(buffer[count]); //a to z
                     else      EndFlag:=true;
                   end;
                   if NOT EndFlag then inc(count);
                 until EndFlag OR (count>(DataPosition+DataLength));
                 if EndFlag AND (length(filename)>0) then
                 begin
                   EndFlag:=false;
                   { find the first char that's a number (file length)}
                   repeat
                     inc(Count);
                   until    ((buffer[count]>=48) AND (buffer[count]<=57))
                         OR (count>(DataPosition+DataLength));
                   if (buffer[count]>=48) AND (buffer[count]<=57) then
                   begin
                     { read the file length from packet zero }
                     StringofNumbers:='';
                     repeat
                       case buffer[count] of
                         48..57  : StringofNumbers:=StringofNumbers+chr(buffer[count]); //0 to 9
                         else      EndFlag:=true;
                       end;
                       if NOT EndFlag then inc(count);
                     until EndFlag OR (count>(DataPosition+DataLength));
                     if EndFlag AND (length(StringofNumbers)>0) then FileLength:=StrToInt(StringofNumbers);
                     {$IFDEF output2screen}
                     ConsoleWindowWriteLn (WindowHandle,FileName);
                     ConsoleWindowWriteLn (WindowHandle,'FileLength:'+IntToStr(FileLength));
                     {$ENDIF}
                     { if we have a useful filename and length then open a file }
                     { initially add .new on the end }
                     { if something goes wrong the original file remains on the SD card }
                     if (length(FileName)>0) AND (FileLength>0) then
                     begin
                       while not DirectoryExists('C:\') do
                       begin
                         {Sleep for a second}
                         Sleep(1000);
                       end;
                       FilenameNew:='C:\'+Filename+'.new';
                       if FileExists(FilenameNew) then
                       begin
                         {If it does exist we can delete it}
                         {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'Deleting the file ' + FilenameNew); {$ENDIF}
                         DeleteFile(FilenameNew);
                       end;
                       try
                         {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'Creating the file ' + FilenameNew);  {$ENDIF}
                         FileStream:=TFileStream.Create(FilenameNew,fmCreate);
                         FileState:=FS_FileOpen;
                         lastPacketNumber:=0;
                         Buffer[1]:=ACK;
                         SerialDeviceWrite (SerialDevice, @Buffer[1], 1, SERIAL_WRITE_NONE, Count);
                         StartTime:=GetTickCount64;
                       except
                        {Something went wrong creating the file}
                         {$IFDEF output2screen} ConsoleWindowWriteLn(WindowHandle,'Failed to create the file ' + FilenameNew); {$ENDIF}
                         Buffer[1]:=NAK;
                         SerialDeviceWrite (SerialDevice, @Buffer[1], 1, SERIAL_WRITE_NONE, Count);
                         StartTime:=GetTickCount64;
                       end;
                       { send a C back to the client }
                       Buffer[1]:=C;
                       SerialDeviceWrite (SerialDevice, @Buffer[1], 1, SERIAL_WRITE_NONE, Count);
                       StartTime:=GetTickCount64;
                     end;
                   end;
                 end;
               end;
            FS_FileOpen:
               { load 128 or 1024 length blocks }
               if    (PacketNumber=(lastPacketNumber+1))
                  OR ((LastPacketNumber=255) AND (PacketNumber=0))
                 then
               begin
                 LastPacketNumber:=PacketNumber;
                 {$IFDEF output2screen}
                 ConsoleWindowWriteLn (WindowHandle,'PacketNumber:'+inttostr(PacketNumber));
                 ConsoleWindowWriteLn (WindowHandle,'DataPosition:'+inttostr(DataPosition));
                 ConsoleWindowWriteLn (WindowHandle,'DataLength:'+inttostr(DataLength));
                 {$ENDIF}
                 { copy bytes into memory }
                 { the last block has padding, don't copy that }
                 BlockInMemory:=TMemoryStream.Create;
                 for count:=DataPosition+1 to DataPosition+Datalength do
                 begin
                   { Don't copy more that FileLength, it's padding }
                   if FileLength>LengthWritten then
                   begin
                     BlockInMemory.WriteByte(Buffer[Count]);
                     inc(LengthWritten);
                   end;
                 end;
                 {$IFDEF output2screen}
                 if LengthWritten>=FileLength then ConsoleWindowWriteLn (WindowHandle,'LengthWritten:'+inttostr(LengthWritten)+' FileLength:'+inttostr(FileLength));
                 ConsoleWindowWriteLn (WindowHandle,'Saving block:'+inttostr(PacketNumber));
                 {$ENDIF}
                 { save memory to file }
                 BlockInMemory.SaveToStream(FileStream);
                 BlockInMemory.Free;
                 { flag and clean up if we have all of it }
                 if LengthWritten>=FileLength then
                 begin
                   FileStream.Free;
                   FileState:=FS_FileFinished;
                 end;
                 { reply with ACK }
                 Buffer[1]:=ACK;
                 SerialDeviceWrite (SerialDevice, @Buffer[1], 1, SERIAL_WRITE_NONE, Count);
                 StartTime:=GetTickCount64;
               end
               else //packetnumber not next in sequence
               begin
                 { we lost a block so quit}
                 {$IFDEF output2screen}
                 ConsoleWindowWriteLn (WindowHandle,'Packet number sequence failure:-');
                 ConsoleWindowWriteLn (WindowHandle,'Last PacketNumber:'+inttostr(lastPacketNumber));
                 ConsoleWindowWriteLn (WindowHandle,'This PacketNumber:'+inttostr(PacketNumber));
                 {$ENDIF}
                 Done:=true;
               end;
          end;
        end
        else //verifypacket failed
        begin
          { something went wrong }
          {$IFDEF output2screen} ConsoleWindowWriteLn (WindowHandle,'Verifypacket=fail'); {$ENDIF}
          { ask client to resend block }
          Buffer[1]:=NAK;
          SerialDeviceWrite (SerialDevice, @Buffer[1], 1, SERIAL_WRITE_NONE, Count);
          StartTime:=GetTickCount64;
        end;
        { block is either saved (if good) or dumped (if bad) so reset packet buffer etc }
        Packetlength:=0; //start again
        PacketFound:=false;
        MinLength:=1029;
      end;
      EndTime:=GetTickCount64;
    until ((EndTime-StartTime)>TimeLimit) OR Done;
    { in case client didn't EOT or something went wrong }
    if FileState=FS_FileOpen then
    begin
      FileStream.Free;
      FileState:=FS_FileFinished;
    end;
    if FileState=FS_FileCopied then Done:=true;
  end; //while not done
end;


function TSerialUpdaterThread.VerifyPacket(PPacket: PByte; out APacketNumber: LongWord; out ADataPosition: LongWord; out ADataLength: LongWord): boolean;
var
  Count: word=0;
begin
  Result:=false;
  Count:=0;
  repeat
    if ((PPacket+Count)^=SOH) OR ((PPacket+Count)^=STX) then
    begin
      if ((PPacket+Count+1)^+(PPacket+Count+2)^)=$FF then
      begin
        APacketNumber:=(PPacket+Count+1)^;
        ADataPosition:=Count+3;
        case (PPacket+Count)^ of
          SOH: ADatalength:=128; //CRC is 132,133;
          STX: ADatalength:=1024;//CRC is 1028, 1029
        end;
        if (((PPacket+Count+ADatalength+3)^ SHL 8) OR (PPacket+Count+ADatalength+4)^) //(131 shl 8) OR 132
           = calcCRC(PPacket+ADataPosition,ADatalength) then
           Result:=true;
      end;
    end;
    if Result=false then inc(Count);
  until Result OR (Count>400);
end;

function TSerialUpdaterThread.calcCRC(const input_str:PByte; num_bytes:integer): word;inline;
//stolen from https://forum.lazarus.freepascal.org/index.php/topic,38279.msg259715.html#msg259715
var
 i,j,crc,c:word;
 ptr:PByte;
 a:integer;
 start_value:word;

begin
  start_value:=0;//CRC_START_XMODEM;
  if not crc_tabccitt_init then
  begin
    for i := 0 to 255 do
    begin
          crc := 0;
          c   := i << 8;
          for j :=0 to 7 do
          begin
            if (crc xor c) and $8000 <> 0 then
              crc := ( crc << 1 ) xor CRC_POLY_CCITT
            else
              crc := crc << 1;
        c := c << 1;
          end;
          crc_tabccitt[i] := crc;
    end;
    crc_tabccitt_init := true;
  end;
  Result := start_value;
  ptr := input_str;
  if ptr <> nil then for  a := 0 to pred(num_bytes) do
  begin
    Result := (Result << 8) xor crc_tabccitt[ ((Result >> 8) xor ptr^) and $00FF ];
        inc(ptr);
  end;
end;

{ this is in the same thread as the unit that includes this unit }
begin
  updater:=TSerialUpdaterThread.Create(False,THREAD_STACK_DEFAULT_SIZE);
end.
