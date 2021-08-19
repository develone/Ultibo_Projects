program fcltest;

{$mode objfpc}{$H+}

uses
  //RaspberryPi,
  QEMUVersatilePB,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
 HTTP,         {Include HTTP and WebStatus so we can see from a web browser what is happening}
 WebStatus,
  Classes,
  Console,
  GraphicsConsole,
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
 Syscalls,
 FileSystem,  {Include the file system core and interfaces}
 FATFS,       {Include the FAT file system driver}
 MMC,         {Include the MMC/SD core to access our SD card}

  FPimage,
  FPReadPNG,

  uCanvas,

  FrameBuffer,
  freetypeh,
  Ultibo;
  {$linklib dwtlift}
  {$linklib libm}
procedure decom_test(x0,y0,x1,y1:LongWord;fn:string); cdecl; external 'libdwtlift' name 'decom_test';
  // Add your unit created from Bin2Type data
  //MyData

  { Add additional units here }
type
 TBinaryData = array[0..1971] of Byte;

var
 BinaryData:TBinaryData = (
  $FF,$4F,$FF,$51,$00,$2F,$00,$00,$00,$00,$01,$00,$00,$00,$01,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$01,$00,$00,$00,$01,$00,
  $00,$00,$00,$00,$00,$00,$00,$00,$00,$03,$07,$01,$01,$07,$01,$01,
  $07,$01,$01,$FF,$52,$00,$0C,$00,$00,$00,$01,$00,$05,$04,$04,$00,
  $01,$FF,$5C,$00,$13,$40,$40,$48,$48,$50,$48,$48,$50,$48,$48,$50,
  $48,$48,$50,$48,$48,$50,$FF,$64,$00,$25,$00,$01,$43,$72,$65,$61,
  $74,$65,$64,$20,$62,$79,$20,$4F,$70,$65,$6E,$4A,$50,$45,$47,$20,
  $76,$65,$72,$73,$69,$6F,$6E,$20,$32,$2E,$32,$2E,$30,$FF,$90,$00,
  $0A,$00,$00,$00,$00,$07,$35,$00,$01,$FF,$93,$CF,$99,$88,$13,$7D,
  $3A,$83,$25,$A6,$A7,$63,$88,$CE,$CD,$9D,$9E,$B8,$47,$81,$39,$D5,
  $E8,$8A,$EA,$A3,$61,$EA,$4C,$32,$87,$B1,$45,$2F,$D7,$02,$43,$9E,
  $EC,$5E,$A5,$8F,$3C,$0E,$5B,$AA,$9D,$86,$65,$9D,$08,$6F,$F3,$CF,
  $95,$48,$28,$4D,$F2,$57,$82,$B6,$F0,$86,$24,$39,$08,$61,$99,$CF,
  $EC,$95,$B5,$C7,$BD,$E7,$7F,$95,$EA,$8F,$87,$FB,$AC,$14,$55,$E3,
  $40,$CC,$B8,$64,$E4,$A5,$45,$B5,$D1,$E1,$8E,$CF,$A1,$60,$4B,$F2,
  $3A,$CB,$AC,$4E,$BA,$21,$7A,$CC,$4B,$8F,$78,$49,$4B,$0B,$9F,$B0,
  $33,$AC,$F5,$BF,$C7,$ED,$39,$9A,$38,$09,$98,$47,$45,$42,$1E,$3F,
  $8A,$89,$BD,$C3,$86,$3E,$C5,$FC,$CD,$2C,$CF,$99,$5C,$7C,$87,$A3,
  $E3,$36,$33,$E9,$3C,$CE,$8C,$6B,$23,$E1,$FD,$CE,$25,$BB,$A0,$3F,
  $F5,$6A,$2A,$8F,$31,$BC,$4D,$AE,$1A,$73,$6E,$C8,$BF,$5F,$64,$46,
  $65,$25,$D7,$16,$B8,$33,$52,$13,$15,$4E,$1C,$E1,$63,$28,$BD,$31,
  $B9,$74,$79,$79,$63,$38,$DD,$64,$47,$AA,$65,$CD,$81,$5E,$0E,$34,
  $D0,$5C,$86,$80,$FE,$37,$19,$F2,$AB,$0D,$74,$44,$D4,$CB,$98,$67,
  $2E,$63,$9C,$55,$A2,$44,$29,$AA,$38,$9F,$33,$C2,$DB,$76,$8D,$BF,
  $5C,$2E,$AB,$F8,$00,$60,$CF,$95,$24,$7C,$68,$63,$E4,$3C,$33,$DE,
  $C0,$CE,$07,$64,$A5,$70,$C8,$9D,$00,$58,$D3,$8E,$D8,$07,$97,$41,
  $30,$86,$95,$7F,$4D,$9B,$16,$13,$3E,$93,$EA,$2F,$3B,$B8,$2D,$37,
  $FF,$5B,$29,$26,$81,$2F,$A8,$D6,$C4,$9F,$57,$84,$9A,$81,$CD,$06,
  $77,$5A,$F2,$02,$A0,$F5,$BA,$9C,$95,$BD,$F4,$49,$F3,$20,$A2,$8D,
  $12,$E9,$6C,$26,$06,$B2,$A2,$E5,$36,$C7,$31,$13,$64,$0B,$50,$0A,
  $12,$8C,$F0,$55,$8F,$CA,$F9,$6C,$40,$2D,$D6,$EA,$2F,$A0,$C6,$D1,
  $93,$C7,$C8,$96,$1F,$03,$90,$F8,$58,$33,$DE,$A4,$C1,$85,$97,$A5,
  $6D,$C8,$C0,$F2,$4C,$93,$8C,$B4,$00,$61,$C5,$33,$6D,$5B,$DD,$3A,
  $4B,$AE,$17,$33,$F7,$5F,$FB,$A7,$D9,$B0,$1C,$83,$85,$3B,$26,$3D,
  $1A,$22,$68,$DC,$4D,$58,$FB,$55,$A2,$4D,$B0,$FB,$86,$16,$DE,$D0,
  $9B,$DC,$22,$00,$50,$EF,$26,$18,$3A,$FE,$25,$F1,$36,$C3,$61,$52,
  $3F,$82,$E4,$71,$21,$BA,$CA,$4E,$66,$02,$B6,$AA,$77,$52,$1E,$F9,
  $95,$F7,$CF,$92,$B9,$1F,$0D,$8C,$7C,$36,$E0,$93,$25,$F4,$AD,$28,
  $14,$EF,$9C,$0E,$DD,$5F,$AB,$A2,$9B,$E8,$67,$19,$45,$00,$D2,$B4,
  $56,$0C,$FD,$5C,$25,$48,$93,$43,$AF,$56,$3E,$B3,$65,$23,$20,$10,
  $34,$9D,$5D,$08,$4A,$2D,$2B,$D0,$55,$4A,$8F,$6A,$41,$17,$19,$3C,
  $9B,$3B,$87,$AB,$D1,$7D,$6E,$A9,$05,$DF,$DC,$F0,$10,$B2,$23,$A4,
  $62,$2B,$F4,$58,$29,$F5,$9A,$3C,$FB,$1D,$C3,$89,$29,$54,$03,$6B,
  $2E,$A4,$D0,$A3,$41,$FE,$87,$52,$F9,$F2,$3A,$00,$6A,$95,$A1,$68,
  $F7,$80,$59,$08,$A2,$75,$3A,$7D,$13,$15,$5D,$59,$DA,$63,$DC,$E6,
  $94,$8A,$17,$C7,$FD,$85,$56,$0C,$98,$E9,$00,$93,$3F,$81,$3B,$AF,
  $A1,$08,$41,$04,$89,$0F,$83,$88,$52,$89,$2F,$1F,$4E,$C1,$4B,$3C,
  $85,$BE,$7A,$BD,$24,$23,$9A,$68,$D8,$D9,$9F,$9F,$62,$53,$C1,$6B,
  $88,$23,$02,$67,$7A,$9E,$9F,$27,$D4,$FE,$B3,$9A,$DB,$EF,$9E,$C0,
  $B2,$CD,$8A,$AE,$61,$F4,$45,$D3,$AF,$42,$4E,$99,$2D,$7A,$CC,$CF,
  $8E,$C3,$1F,$0D,$CC,$7C,$37,$A0,$54,$72,$65,$A3,$EB,$20,$89,$66,
  $6A,$3A,$A1,$96,$17,$FC,$CA,$D2,$61,$30,$DA,$9C,$C0,$93,$03,$F6,
  $22,$3A,$EE,$3F,$0D,$4C,$DE,$1D,$4B,$EF,$A9,$D5,$FF,$67,$04,$19,
  $CB,$E8,$F3,$0F,$43,$C6,$BD,$2D,$01,$3B,$D8,$DB,$22,$28,$C0,$A1,
  $19,$B0,$57,$8F,$79,$CF,$79,$A2,$DF,$3A,$21,$DC,$A8,$C5,$03,$E9,
  $CE,$69,$4F,$F8,$DB,$A2,$5C,$55,$39,$B5,$7D,$20,$8F,$76,$1D,$2F,
  $BB,$13,$7E,$D1,$DC,$15,$8E,$D0,$86,$52,$EA,$E2,$1B,$03,$46,$4E,
  $73,$54,$15,$51,$BB,$80,$38,$19,$A1,$95,$B3,$C2,$A7,$7E,$1B,$DF,
  $B0,$D5,$31,$87,$DB,$F4,$36,$34,$66,$90,$98,$BB,$44,$0C,$29,$E2,
  $80,$0E,$EC,$E1,$CC,$83,$48,$BB,$BE,$15,$BB,$F7,$26,$A4,$23,$A0,
  $48,$6D,$41,$A9,$95,$0F,$1A,$1F,$2C,$62,$7B,$8D,$07,$8E,$32,$00,
  $91,$22,$AE,$3B,$4A,$48,$BF,$9F,$85,$50,$48,$3B,$02,$A9,$69,$79,
  $AD,$35,$C6,$63,$8B,$5B,$8C,$28,$B9,$A6,$DB,$A4,$AF,$A5,$D4,$8A,
  $1E,$E0,$16,$EE,$33,$20,$38,$13,$3F,$27,$EC,$7B,$92,$DB,$9B,$C7,
  $C3,$A5,$C7,$C3,$5B,$1F,$0D,$58,$54,$71,$85,$CB,$3E,$A3,$D2,$FB,
  $D0,$8B,$04,$75,$B8,$B5,$E2,$46,$59,$3E,$8D,$24,$70,$37,$AF,$93,
  $BE,$6E,$1E,$83,$4A,$92,$31,$12,$51,$56,$13,$9C,$67,$58,$FE,$C3,
  $48,$87,$11,$4D,$D1,$ED,$56,$A8,$36,$E4,$FE,$30,$57,$44,$A0,$47,
  $D2,$FE,$A6,$34,$C1,$E9,$11,$91,$D3,$79,$48,$F4,$6E,$52,$9F,$98,
  $E5,$E4,$8A,$52,$FA,$03,$9F,$87,$81,$E1,$AC,$BB,$E1,$BE,$7D,$45,
  $AC,$B0,$8C,$EA,$CE,$CB,$F6,$0B,$3D,$9F,$1D,$DE,$99,$08,$E3,$EA,
  $11,$81,$6C,$83,$EA,$0D,$0A,$C6,$71,$86,$9F,$09,$9C,$2C,$A4,$60,
  $41,$A9,$AB,$ED,$7C,$4E,$A2,$01,$9E,$FE,$B4,$1A,$3B,$B5,$B8,$E4,
  $9D,$15,$52,$36,$EF,$49,$56,$21,$9F,$A1,$8F,$46,$D0,$1C,$1B,$5F,
  $78,$5B,$BD,$5E,$4C,$FC,$C8,$96,$63,$8C,$51,$CF,$87,$A1,$A3,$96,
  $63,$B5,$A0,$DB,$5E,$81,$4B,$CE,$F8,$5A,$91,$E5,$C7,$B1,$55,$F8,
  $3D,$A8,$7C,$5C,$84,$F5,$D0,$74,$D3,$A6,$C1,$B7,$E3,$C3,$E4,$AE,
  $9F,$CB,$2F,$48,$DE,$A2,$D7,$23,$44,$21,$E1,$33,$69,$C5,$03,$29,
  $B2,$57,$39,$38,$BE,$92,$F2,$8F,$88,$2F,$5F,$92,$30,$DF,$32,$E9,
  $11,$4B,$32,$E6,$70,$51,$26,$17,$74,$32,$E9,$2E,$CF,$AA,$7C,$62,
  $D5,$B3,$D4,$7C,$A2,$5B,$7F,$82,$84,$97,$2D,$1A,$CC,$32,$AA,$D3,
  $B7,$18,$6D,$AB,$5C,$3C,$A8,$7B,$2D,$C8,$9E,$85,$7A,$46,$D6,$41,
  $B4,$36,$C4,$31,$72,$92,$9D,$8A,$F0,$4A,$2F,$B7,$C2,$B8,$5B,$B6,
  $E7,$30,$05,$99,$16,$5B,$80,$42,$93,$C0,$31,$3E,$EF,$6B,$64,$23,
  $67,$E4,$CC,$E4,$AD,$A7,$FC,$11,$FA,$BF,$5B,$6C,$52,$CF,$3E,$B9,
  $AE,$D8,$A5,$0A,$8A,$42,$3C,$35,$6F,$95,$DC,$D6,$F9,$6B,$D4,$8B,
  $47,$1C,$AE,$E7,$CF,$2E,$9B,$31,$3D,$CA,$21,$46,$CF,$F1,$05,$3F,
  $35,$A6,$05,$D0,$D9,$31,$88,$DC,$87,$45,$E8,$34,$CB,$E7,$68,$CF,
  $87,$A3,$E7,$C3,$A2,$CF,$86,$F4,$A1,$DD,$51,$A7,$9E,$60,$F5,$A4,
  $75,$34,$EF,$2C,$67,$9D,$49,$30,$EE,$95,$3E,$C0,$00,$06,$5B,$2D,
  $5A,$30,$E5,$4B,$FC,$12,$B4,$3A,$F0,$BE,$FC,$43,$B6,$58,$F2,$4C,
  $DC,$AF,$ED,$52,$8D,$F9,$32,$51,$65,$88,$57,$11,$0C,$49,$67,$3B,
  $71,$A5,$40,$81,$97,$0B,$71,$8B,$82,$73,$46,$25,$D4,$3F,$3D,$C1,
  $34,$62,$EE,$4B,$C2,$84,$CF,$22,$6B,$52,$92,$10,$57,$C2,$E4,$BF,
  $2F,$DB,$31,$35,$82,$74,$C8,$FB,$2D,$93,$6D,$1D,$78,$09,$2D,$8E,
  $35,$27,$16,$08,$69,$D6,$50,$BE,$46,$1F,$56,$35,$92,$72,$51,$7C,
  $54,$C1,$A1,$68,$2A,$DA,$70,$F7,$CC,$60,$F1,$58,$B3,$A7,$16,$A7,
  $C1,$0F,$32,$9C,$77,$F9,$F8,$C1,$CB,$F0,$69,$33,$28,$65,$C0,$CB,
  $A1,$D8,$6A,$05,$4A,$B3,$77,$C6,$82,$62,$00,$58,$13,$10,$A3,$72,
  $4E,$A1,$2B,$A1,$01,$DA,$82,$BA,$44,$7B,$86,$C5,$13,$0A,$E7,$1F,
  $D6,$13,$18,$B9,$31,$F1,$FF,$37,$DC,$86,$F6,$02,$F3,$26,$55,$55,
  $A0,$B7,$C2,$96,$39,$9B,$CD,$6A,$FE,$EF,$68,$EE,$C2,$15,$96,$3C,
  $1D,$ED,$E7,$67,$1A,$62,$84,$B6,$23,$85,$2A,$8E,$6A,$72,$53,$1F,
  $3D,$8F,$74,$A6,$3A,$7A,$93,$B4,$CD,$06,$34,$6B,$E6,$54,$DB,$A2,
  $97,$00,$41,$3D,$D8,$ED,$C3,$3A,$45,$49,$C8,$28,$C3,$00,$95,$B5,
  $DC,$DF,$84,$78,$81,$A0,$DF,$9A,$DD,$C7,$74,$D8,$ED,$4C,$75,$E0,
  $A1,$DA,$AF,$27,$A3,$7A,$A7,$8E,$8E,$15,$AC,$D3,$C4,$49,$F0,$61,
  $D4,$0F,$83,$FB,$8E,$79,$F9,$4A,$2A,$50,$F3,$12,$3A,$21,$98,$15,
  $C6,$DA,$86,$37,$4A,$D8,$A7,$18,$25,$FF,$5C,$54,$44,$4C,$6F,$80,
  $17,$7B,$34,$F4,$32,$9A,$97,$72,$34,$49,$3A,$8C,$6B,$39,$65,$62,
  $8B,$38,$A8,$16,$AA,$61,$A0,$FE,$DF,$D4,$7C,$08,$D4,$C1,$CB,$0D,
  $60,$DE,$19,$D4,$BF,$A2,$C8,$1F,$DE,$4B,$12,$0E,$F8,$8A,$02,$C5,
  $72,$34,$10,$B3,$17,$91,$B1,$25,$5F,$E4,$3C,$AE,$B3,$8A,$0A,$08,
  $C2,$3F,$64,$4C,$C8,$A0,$C2,$15,$96,$3A,$A6,$3F,$E0,$11,$53,$88,
  $D5,$10,$A8,$C2,$98,$C2,$AE,$DD,$AC,$40,$62,$7D,$DC,$BA,$A9,$15,
  $86,$D2,$74,$8D,$80,$CE,$EE,$C0,$E4,$92,$E0,$0E,$EA,$D3,$9C,$17,
  $AC,$CA,$25,$04,$EF,$16,$0F,$0F,$50,$08,$91,$56,$A7,$2D,$13,$8D,
  $02,$96,$E8,$83,$5D,$F5,$DA,$66,$C9,$B5,$C8,$B2,$92,$9A,$42,$A8,
  $3D,$74,$EF,$93,$4F,$C0,$AF,$EF,$07,$E8,$B7,$EE,$5D,$16,$F4,$9C,
  $48,$16,$D7,$1B,$AD,$E0,$32,$5F,$3F,$0D,$7E,$1A,$E4,$61,$F7,$A5,
  $4F,$4E,$91,$08,$10,$9B,$08,$17,$EF,$42,$43,$AB,$9C,$47,$8F,$EB,
  $7D,$65,$25,$96,$AD,$72,$8F,$2F,$7B,$C9,$42,$1A,$3B,$C2,$5E,$CA,
  $C0,$AD,$57,$E4,$9D,$4B,$4A,$3A,$8A,$EA,$63,$8F,$E5,$65,$80,$80,
  $80,$80,$FF,$D9
 );



// Add the new class
type
  TBufferStream = class(TCustomMemoryStream)
  public
    constructor Create(ABuffer: Pointer; ASize: PtrInt);
  end;

constructor TBufferStream.Create(ABuffer: Pointer; ASize: PtrInt);
begin
  inherited Create;

  SetPointer(ABuffer, ASize);
end;

const
  BACK_COLOUR                    = $FF055A93;

var
 Count:Integer;
 Filename:String;
 SearchRec:TSearchRec;
 StringList:TStringList;
 FileStream:TFileStream;
 WindowHandle:TWindowHandle;
 CR, enc, xx0, yy0, xx1, yy1:LongWord;
 MyPLoggingDevice : ^TLoggingDevice;
  Handle:THandle;
 Handle1:THandle;
  GConsole : TWindowHandle;
 Window:TWindowHandle;
 HTTPListener:THTTPListener;
 BGnd, aCanvas : TCanvas;
 BufferStream: TBufferStream;
 IPAddress : string;
  anImage0 : TFPCustomImage;
  DefFrameBuff : PFrameBufferDevice;
  Properties : TWindowProperties;

 DECOMP: Integer;
 ENCODE: Integer;
 TCP_DISTORATIO: Integer;
 FILTER: Integer;
 COMPRESSION_RATIO : Integer;
 DIS_CR_FLG : Integer;
 X:LongWord;
 Y:LongWord;
 Width:LongWord;
 Height:LongWord;
 da_x0,da_y0,da_x1,da_y1:LongWord;
 ff:string;

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

  ConsoleWindowWriteLn (Handle1, s);

end;

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

begin
{
 The following 3 lines are logging to the console
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
 }

 {The following 2 lines are logging to a file
 LoggingDeviceSetTarget(LoggingDeviceFindByType(LOGGING_TYPE_FILE),'c:\ultibologging.log');
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_FILE));
 MyPLoggingDevice:=LoggingDeviceGetDefault;
 LoggingDeviceRedirectOutput(MyPLoggingDevice); }


 // wait for IP address and SD Card to be initialised.
 WaitForSDDrive;
 IPAddress := WaitForIPComplete;
 {Create and start the HTTP Listener for our web status page}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 {Wait a few seconds for all initialization (like filesystem and network) to be done}
 Sleep(5000);
 {Register the web status page, the "Thread List" page will allow us to see what is happening in the example}
 WebStatusRegister(HTTPListener,'','',True);
 {Create a graphics window to display our bitmap, let's use the new CONSOLE_POSITION_FULLSCREEN option}
 Window:=GraphicsWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT);
  Handle:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);
  Handle1:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);
  GConsole := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOMRIGHT, true);
  ConsoleWindowWriteLn(Handle, TimeToStr(Time));
  ConsoleWindowWriteLn (Handle1, 'TFTP Demo.');

  DefFrameBuff := FramebufferDeviceGetDefault;
  DECOMP:=6;
 ENCODE:=1;
  //FILTER 0 5/3 DWT
 //FILTER 1 9/7 DWT
 FILTER:= 0;
 COMPRESSION_RATIO := 125;

 ConsoleWindowWriteLn(Handle, 'ENCODE: ' + intToStr(ENCODE));
 ConsoleWindowWriteLn(Handle, 'sizeof ENCODE: ' + intToStr(sizeof(ENCODE)));
  da_x0:=0;
 da_y0:=0;
 da_x1:=2048;
 da_y1:=2048;
 ff:='test.j2k';
 //rd_inps();
 {starting the procedure to read the file testfile which contains a struct
 which has the varables for decompression.}
  try
  Filename:='C:\testfile';
  try
   FileStream:=TFileStream.Create(Filename,fmOpenRead);
   FileStream.Read(CR,sizeof(CR));
   COMPRESSION_RATIO:=CR;
   ConsoleWindowWriteLn(Handle, 'xx0 ' + intToStr(xx0));
   FileStream.Read(enc,sizeof(enc));
   ENCODE:=enc;
   ConsoleWindowWriteLn(Handle, 'xx0 ' + intToStr(xx0));
   FileStream.Read(xx0,sizeof(xx0));
   da_x0:=xx0;
   ConsoleWindowWriteLn(Handle, 'xx0 ' + intToStr(xx0));
   FileStream.Read(yy0,sizeof(yy0));
   da_y0:=yy0;
   ConsoleWindowWriteLn(Handle, 'yy0 ' + intToStr(yy0));
   FileStream.Read(xx1,sizeof(xx1));
   da_x1:=xx1;
   ConsoleWindowWriteLn(Handle, 'xx1 ' + intToStr(xx1));
   FileStream.Read(yy1,sizeof(yy1));
   da_y1:=yy1;
   ConsoleWindowWriteLn(Handle, 'yy1 ' + intToStr(yy1));
  finally
  end;
 except
   on E: Exception do
   begin
     ConsoleWindowWriteLn(Handle, 'Error: ' + E.Message);
    end;
 end;

 //should not be set lower than  30 which is compressiong over 1500
 //
 //		38	189.4093899116
 //		44	44.058396563
 //		50	15.9377967826
 //		54	8.6079098426
 //		58	6.0368784486
 //		60	5.5454244973

 TCP_DISTORATIO:=60;
 //DIS_CR_FLG 0 COMPRESSION_RATIO
 //DIS_CR_FLG 1 TCP_DISTORATIO
 DIS_CR_FLG := 0;
 if (ENCODE = 1) then
  ConsoleWindowWriteLn(Handle, 'dummy space holder');
 {DrawBitmap(Window,'C:\MyBitmap.bmp',0,0,DECOMP,ENCODE,TCP_DISTORATIO,FILTER, COMPRESSION_RATIO,DIS_CR_FLG);|BufferStream: TBufferStream;}

 end.
 if(ENCODE = 0) then
 begin

 decom_test(da_x0,da_y0,da_x1,da_y1,ff);
 {DrawBitmap(Window,'C:\test_wr.bmp',0,0,DECOMP,ENCODE,TCP_DISTORATIO,FILTER, COMPRESSION_RATIO,DIS_CR_FLG);}
  WaitForSDDrive;



  aCanvas := TCanvas.Create;
  if GraphicsWindowGetProperties (GConsole, @Properties) = ERROR_SUCCESS then
    begin
      aCanvas.Left := Properties.X1;
      aCanvas.Top := Properties.Y1;
      aCanvas.SetSize (Properties.X2 +1 - Properties.X1 , Properties.Y2 + 1  - Properties.Y1  , COLOR_FORMAT_ARGB32);
   end;
  aCanvas.Fill (BACK_COLOUR);
  aCanvas.Flush (DefFrameBuff);

  anImage0 := TFPMemoryImage.Create (0, 0);

  // Remove the LoadFromFile
  //anImage0.LoadFromFile ('C:\background.png');

  // Add a LoadFromStream
  BufferStream := TBufferStream.Create(@BinaryData, SizeOf(BinaryData));
  anImage0.LoadFromStream(BufferStream);

  BGnd := TCanvas.Create;
  BGnd.SetSize (aCanvas.Width, aCanvas.Height, aCanvas.ColourFormat);

  aCanvas.DrawImage (anImage0, 0, 0, BGnd.Width, BGnd.Height);

  aCanvas.Flush (DefFrameBuff);


  ThreadHalt (0);
end.

