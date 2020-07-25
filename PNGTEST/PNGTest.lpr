program PNGTest;

{$mode objfpc}{$H+}

{$define use_tftp}

{ Raspberry Pi 3 Application                                                   }
{  Add your program code below, add additional units to the "uses" section if  }
{  required and create new units by selecting File, New Unit from the menu.    }
{                                                                              }
{  To compile your program select Run, Compile (or Run, Build) from the menu.  }

uses
  RaspberryPi3,
  GlobalConfig,
  GlobalConst,
  GlobalTypes,
  Platform,
  Threads,
  SysUtils,
  Console,
  GraphicsConsole,
  Classes, uLog,
  UltiboClasses,
  FrameBuffer, uFontInfo, freetypeh,
{$ifdef use_tftp}
  uTFTP, Winsock2,
{$endif}
  Ultibo, uPng, uCanvas
  { Add additional units here };

type
  TPngData = record
    Png : TPng;
    Frame : integer;
    x, y : integer;
  end;
  PPngData = ^TPngData;

  { THelper }

  THelper = class
    procedure DoTimer (Sender : TObject);
  end;

var
  Console1, Console2, Console3 : TWindowHandle;
  ch : char;
{$ifdef use_tftp}
  IPAddress : string;
{$endif}
  FBFormat : LongWord;
  FrameProps : TFrameBufferProperties;
  Canvas : TCanvas;
  DefFrameBuff : PFrameBufferDevice;
  Rect : Ultibo.TRect;
  cRect : TConsoleRect;
  Pngs : TList;
  PngData : PPngData;
  Timer : TTimerEx;
  Helper : THelper;
  s : string;
  si, sc : integer;

procedure Log1 (s : string);
begin
  ConsoleWindowWriteLn (Console1, s);
end;

procedure Log2 (s : string);
begin
  ConsoleWindowWriteLn (Console2, s);
end;

procedure Msg2 (Sender : TObject; s : string);
begin
  Log2 ('TFTP - ' + s);
end;

procedure WaitForSDDrive;
begin
  while not DirectoryExists ('C:\') do sleep (500);
end;

procedure DrawNextPng;
var
  aPngData : PPngData;
  i : integer;
  t : string;
begin
  Canvas.Fill ($ff000080);
  if (si > 0) then
    begin
      t := Copy (s, 1, si);
      Canvas.DrawText (20, 40, t, 'arial', 24, COLOR_WHITE, 200);
    end;
  for i := 0 to Pngs.Count - 1 do
    begin
      aPngData := PPngData (Pngs[i]);
      aPngData^.Frame := aPngData^.Frame + 1;
      if aPngData^.Frame  > aPngData^.Png.NosFrames then aPngData^.Frame := 1;
      aPngData^.Png.Draw (Canvas, SetRect (aPngData^.x, aPngData^.y,
                                           aPngData^.x + aPngData^.Png.Width,
                                           aPngData^.y + aPngData^.Png.Height), aPngData^.Frame);

    end;
  if (si > 0) then
    begin
      t := Copy (s, 1, si);
      Canvas.DrawText (20, 100, t, 'arial', 24, COLOR_WHITE, 200);
    end;
  Canvas.Draw (DefFrameBuff, (FrameProps.PhysicalWidth div 2) + 2, (FrameProps.PhysicalHeight div 2) + 2);
end;

{ THelper }

procedure THelper.DoTimer (Sender: TObject);
begin
  if sc > 0 then
    begin
      sc := sc - 1;
      if sc = 0 then
        begin
          sc := 3;
          si := si + 1;
          if si > length (s) then si := 1;
        end;
    end;
  DrawNextPng;
end;

{$ifdef use_tftp}
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
          sleep (1000);
          Result := TCP.LocalAddress;
        end;
    end;
  TCP.Free;
end;
{$endif}

begin
  Console1 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_LEFT, true);
  Console2 := ConsoleWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_TOPRIGHT, false);
  Console3 := GraphicsWindowCreate (ConsoleDeviceGetDefault, CONSOLE_POSITION_BOTTOMRIGHT);
  SetLogProc (@Log1);
  Log1 ('Animated PNG Test.');
  Log1 ('Uses my own version of TCanvas.');
  WaitForSDDrive;

{$ifdef use_tftp}
  IPAddress := WaitForIPComplete;
  Log2 ('TFTP - Enabled.');
  Log2 ('TFTP - Syntax "tftp -i ' + IPAddress + ' PUT kernel7.img"');
  SetOnMsg (@Msg2);
  Log2 ('');
{$endif}

  DefFrameBuff := FramebufferDeviceGetDefault;
  Canvas := TCanvas.Create;
  FrameProps.Size := 0;
  FBFormat := COLOR_FORMAT_UNKNOWN;
  if FramebufferDeviceGetProperties (DefFrameBuff, @FrameProps) = ERROR_SUCCESS then
    begin
      Log ('Buffer Colour Format ' + FrameProps.Format.ToString + ' Depth ' + FrameProps.Depth.ToString + ' Size ' + Frameprops.Size.ToString);
      Log ('Buffer Width ' + FrameProps.PhysicalWidth.ToString + ' Height ' + FrameProps.PhysicalHeight.ToString);
      FBFormat := FrameProps.Format;
    end
  else
    Log ('failed to get props');
  cRect := GraphicsWindowGetRect (Console3);
  Canvas.SetSize (cRect.X2 - cRect.X1, cRect.Y2 - cRect.Y1, FBFormat);

  Pngs := TList.Create;
  New (PngData);
  PngData^.Png := TPng.Create;
  PngData^.Png.LoadFromFile ('gears.png');
  PngData^.Png.RenderAllFrames;
  PngData^.Frame := 0;
  PngData^.x := 20;
  PngData^.y := 20;
  Pngs.Add (PngData);

  New (PngData);
  PngData^.Png := TPng.Create;
  PngData^.Png.LoadFromFile ('ball2.png');
  PngData^.Png.RenderAllFrames;
  PngData^.Frame := 0;
  PngData^.x := 20;
  PngData^.Y := 200;
  Pngs.Add (PngData);

  New (PngData);
  PngData^.Png := TPng.Create;
  PngData^.Png.LoadFromFile ('orangered.png');
  PngData^.Png.RenderAllFrames;
  PngData^.Frame := 0;
  PngData^.x := 200;
  PngData^.Y := 20;
  Pngs.Add (PngData);

  New (PngData);
  PngData^.Png := TPng.Create;
  PngData^.Png.LoadFromFile ('smiley.png');
  PngData^.Png.RenderAllFrames;
  PngData^.Frame := 0;
  PngData^.x := 200;
  PngData^.Y := 200;
  Pngs.Add (PngData);

  New (PngData);
  PngData^.Png := TPng.Create;
  PngData^.Png.LoadFromFile ('orbit.png');
  PngData^.Png.RenderAllFrames;
  PngData^.Frame := 0;
  PngData^.x := 380;
  PngData^.Y := 20;
  Pngs.Add (PngData);

  New (PngData);
  PngData^.Png := TPng.Create;
  PngData^.Png.LoadFromFile ('sanduhr.png');
  PngData^.Png.RenderAllFrames;
  PngData^.Frame := 0;
  PngData^.x := 380;
  PngData^.Y := 200;
  Pngs.Add (PngData);

  s := 'In front and behind gears.';
  si := 0;
  sc := 3;

  DrawNextPng;

  Log2 ('Commands...');
  Log2 ('  1  Start.');
  Log2 ('  2  Stop.');
  Log2 ('');

  Helper := THelper.Create;
  Timer := TTimerEx.Create;
  Timer.Enabled := false;
  Timer.OnTimer := @Helper.DoTimer;
  Timer.Interval := 70;
  Timer.Enabled := true;
  ch := #0;
  while true do
    begin
      if ConsoleGetKey (ch, nil) then
        case (ch) of
          '1' : Timer.Enabled := true;
          '2' : Timer.Enabled := false;
          'M', 'm' :
            begin
              Canvas.Fill (COLOR_GREEN);
              Rect := SetRect (30, 0, 30 + 80, 60);
              Canvas.Fill (Rect, COLOR_RED);
              Rect := SetRect (39, 40, 30 + 39, 20 + 40);
              Canvas.Fill (Rect, COLOR_BROWN);
              Canvas.DrawText (20, 20, 'How is it going', 'arial', 24, COLOR_BLUE);
              Canvas.Draw (DefFrameBuff, (FrameProps.PhysicalWidth div 2) + 2, (FrameProps.PhysicalHeight div 2) + 2);
            end;
        end;
    end;
  Helper.Free;
  ThreadHalt (0);
end.

