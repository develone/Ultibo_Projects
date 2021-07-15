unit uCanvas;

(* Generic off-screen drawing canvas *)
(* 2016-18 pjde *)

{$mode objfpc}{$H+}
{
    Interpolation originally derived from FPCanvas whose copyright message reads :-

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    Basic canvas definitions.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{Tweaked to run on Ultibo Canvas pjde 2018 }
{$H+}

interface

uses
  Classes, SysUtils, FrameBuffer, uFontInfo, Ultibo, freetypeh, FPImage;

type

  TFPCustomInterpolation = class;

{ TCanvas }

  TCanvas = class
  private
    IP : TFPCustomInterpolation;
    function GetFont (byFont : string; Load : boolean) : TFontInfo;
  public
    ColourFormat : LongWord;
    Width, Height : integer;
    Left, Top : integer;
    Buffer : PByteArray;
    BufferSize : integer;
    BitCount : integer;
    Fonts : TList;
    procedure FindFonts;
    procedure SetSize (w, h : integer; cf : LongWord);
    procedure Fill (Col : LongWord); overload;
    procedure Fill (Rect : Ultibo.TRect; Col : LongWord); overload;
    function TextExtents (Text, Font : string; Size : integer) : FT_Vector;
    procedure DrawText (x, y: integer; Text, Font : string; FontSize : integer; Col : LongWord); overload;
    procedure DrawText (x, y: integer; Text, Font : string; FontSize : integer; Col : LongWord; Alpha : byte); overload;
    procedure Flush (FrameBuff : PFrameBufferDevice; x, y : integer); overload;
    procedure Flush (FrameBuff : PFrameBufferDevice); overload;
    procedure Assign (anOther : TCanvas);
    procedure DrawImage (anImage : TFPCustomImage; x, y : integer);  overload;
    procedure DrawImage (anImage : TFPCustomImage; x, y, h, w : integer); overload;
    constructor Create;
    destructor Destroy; override;
  end;

{ TFPCustomInterpolation }

  TFPCustomInterpolation = class
  private
    FCanvas: TCanvas;
    FImage: TFPCustomImage;
  protected
  public
    procedure Initialise (anImage : TFPCustomImage; aCanvas : TCanvas); virtual;
    procedure Execute (x, y, w, h : integer); virtual; abstract;

    property Canvas : TCanvas read fCanvas;
    property Image : TFPCustomImage read fimage;
  end;

{ TFPBaseInterpolation }

  TFPBaseInterpolation = class (TFPCustomInterpolation)
  private
    procedure CreatePixelWeights (OldSize, NewSize : integer;
      out Entries: Pointer; out EntrySize : integer; out Support : integer);
  protected
  public
    procedure Execute (x, y, w, h : integer); override;
    function Filter (x : double) : double; virtual;
    function MaxSupport : double; virtual;
  end;

{ TMitchelInterpolation }

  TMitchelInterpolation = class (TFPBaseInterpolation)
  protected
    function Filter (x : double) : double; override;
    function MaxSupport : double; override;
  end;

function SetRect (Left, Top, Right, Bottom : long) : Ultibo.TRect;
function GetRValue (c : LongWord) : byte;
function GetGValue (c : LongWord) : byte;
function GetBValue (c : LongWord) : byte;
function rgb (r, g, b : byte) : LongWord; inline;

implementation

uses GlobalConst, uLog, Math;

var
  FTLib : PFT_Library;                // handle to FreeType library

function FT_New_Memory_Face (alibrary: PFT_Library; file_base: pointer; file_size: longint; face_index: integer; var face: PFT_Face) : integer; cdecl; external freetypedll Name 'FT_New_Memory_Face';

const
  DPI                           = 72;
{ TCanvas }

function SetRect (Left, Top, Right, Bottom : long) : Ultibo.TRect;
begin
  Result.left := Left;
  Result.top := Top;
  Result.right := Right;
  Result.bottom := Bottom;
end;

function GetRValue (c : LongWord) : byte; inline;
begin
  Result := (c and $ff0000) shr 16;
end;

function GetGValue (c : LongWord) : byte; inline;
begin
  Result :=  (c and $ff00) shr 8;
end;

function GetBValue (c : LongWord) : byte; inline;
begin
  Result := c and $ff;
end;

function rgb (r, g, b : byte) : LongWord; inline;
begin
  Result := $ff000000 + (r shl 16) + (g shl 8) + b;
end;

function TCanvas.TextExtents (Text, Font : string; Size : integer) : FT_Vector;
var
  err : integer;
  aFace : PFT_Face;
  fn : string;
  i: integer;
  kerning : boolean;
  glyph_index,
  prev : cardinal;
  delta : FT_Vector;
  anInfo : TFontInfo;
begin
  Result.x := 0;
  Result.y := 0;
  delta.x := 0;
  delta.y := 0;
  if not Assigned (FTLib) then exit;
  aFace := nil;
  if ExtractFileExt (Font) = '' then
    fn := Font + '.ttf'
  else
    fn := Font;
  anInfo := GetFont (fn, true);
  if anInfo = nil then exit;
  err := FT_New_Memory_Face (FTLIB, anInfo.Stream.Memory, anInfo.Stream.Size, 0, aFace);
  if err = 0 then  // if font face loaded ok
    begin
      err := FT_Set_Char_Size (aFace,                   // handle to face object
             0,                                         // char width in 1/64th of points - Same as height
             Size * 64,                                 // char height in 1/64th of points
             DPI,                                       // horizontal device resolution
             0);                                        // vertical device resolution
      if err = 0 then
        begin
          prev := 0;    // no previous char
          kerning := FT_HAS_KERNING (aFace);
          for i := 1 to length (Text) do
            begin                                       // convert character code to glyph index
              glyph_index := FT_Get_Char_Index (aFace, cardinal (Text[i]));
              if kerning and (prev <> 0) and (glyph_index <> 0) then
                begin
                  FT_Get_Kerning (aFace, prev, glyph_index, FT_KERNING_DEFAULT, &delta);
                  Result.x := Result.x + delta.x;
                  //if aFace^.glyph^.bitmap^.height + aFace^.glyph^.bitmap_top > Result.y then
                    //Result.y := aFace^.glyph^.bitmap^.height + aFace^.glyph^.bitmap_top;
                end;
               // load glyph image into the slot (erase previous one)
               err := FT_Load_Glyph (aFace, glyph_index, FT_LOAD_NO_BITMAP);
               if err > 0 then continue;                // ignore errors
               Result.x := Result.x + aFace^.glyph^.advance.x;
               //if aFace^.glyph^.bitmap^.height + aFace^.glyph^.bitmap_top > Result.y then
                 //Result.y := aFace^.glyph^.bitmap^.height + aFace^.glyph^.bitmap_top;
               prev := glyph_index;
            end;
        end;
      FT_Done_Face (aFace);
    end;
  Result.x := Result.x div 64;
  Result.y := Result.y div 64;
end;

procedure TCanvas.DrawText (x, y : integer; Text, Font : string; FontSize : integer; Col : LongWord; Alpha : byte);
var
  err : integer;
  aFace : PFT_Face;
  fn : string;
  i, tx, ty : integer;
  kerning : boolean;
  glyph_index,
  prev : cardinal;
  delta : FT_Vector;
  anInfo : TFontInfo;

  procedure DrawChar (b : FT_Bitmap; dx, dy : integer);
  var
    i , j : integer;
    x_max, y_max : integer;
    p, q : integer;
    fm : PByte;
    rd, gn, bl : byte;
    cp : PCardinal; // canvas pointer
   begin
    x_max := dx + b.width;
    y_max := dy + b.rows;
//    Log ('dx ' + InttoStr (dx) + ' dy ' +  IntToStr (dy) + ' x max ' +  IntToStr (x_max) + ' y max ' + IntToStr (y_max));
    case ColourFormat of
      COLOR_FORMAT_ARGB32 : {32 bits per pixel Red/Green/Blue/Alpha (RGBA8888)}
        begin
          q := 0;
          for j := dy to y_max - 1 do
            begin
              if (j >= 0) and (j < Height) then
                begin
{$warnings off}
                  cp := PCardinal (LongWord (Buffer) + ((j * Width) + dx) * 4);
{$warnings on}
                  p := 0;
                  for i := dx to x_max - 1 do
                    begin
                      if (i >= 0) and (i < Width) then
                        begin
                          LongWord (fm) := LongWord (b.buffer) + q * b.width + p; // read alpha value of font char
                          fm^ := (fm^ * alpha) div 255;
                          rd := ((GetRValue (Col) * fm^) + (GetRValue (cp^) * (255 - fm^))) div 255;
                          gn := ((GetGValue (Col) * fm^) + (GetGValue (cp^) * (255 - fm^))) div 255;
                          bl := ((GetBValue (Col) * fm^) + (GetBValue (cp^) * (255 - fm^))) div 255;
                          cp^ := rgb (rd, gn, bl);
                        end;
                      p := p + 1;
                      Inc (cp, 1);
                    end;
                  q := q + 1;
                end;
            end;
        end; // colour format
      end; // case
  end;

begin
  if not Assigned (FTLib) then exit;
  aFace := nil;
  tx := x;
  ty := y;
  delta.x := 0;
  delta.y := 0;
  if ExtractFileExt (Font) = '' then
    fn := Font + '.ttf'
  else
    fn := Font;
  anInfo := GetFont (fn, true);
  if anInfo = nil then exit;
  err := FT_New_Memory_Face (FTLIB, anInfo.Stream.Memory, anInfo.Stream.Size, 0, aFace);
  if err = 0 then  // if font face loaded ok
    begin
      err := FT_Set_Char_Size (aFace,                   // handle to face object
             0,                                         // char_width in 1/64th of points - Same as height
             FontSize * 64,                             // char_height in 1/64th of points
             DPI,                                       // horizontal device resolution - dots per inch
             0);                                        // vertical device resolution - dots per inch
      if err = 0 then
        begin
          prev := 0;    // no previous char
          kerning := FT_HAS_KERNING (aFace);
          for i := 1 to length (Text) do
            begin                                       // convert character code to glyph index
              glyph_index := FT_Get_Char_Index (aFace, cardinal (Text[i]));
              if kerning and (prev <> 0) and (glyph_index <> 0) then
                begin
                  FT_Get_Kerning (aFace, prev, glyph_index, FT_KERNING_DEFAULT, &delta);
                  tx := tx + delta.x div 64;
                end;
               // load glyph image into the slot (erase previous one)
               err := FT_Load_Glyph (aFace, glyph_index, FT_LOAD_RENDER);
               if err > 0 then continue;                // ignore errors
               // now draw to our target surface
               DrawChar (aFace^.glyph^.bitmap, tx + aFace^.glyph^.bitmap_left,
                          ty - aFace^.glyph^.bitmap_top);
               tx := tx + aFace^.glyph^.advance.x div 64;
               prev := glyph_index;
            end;
        end;
      FT_Done_Face (aFace);
    end;
end;

procedure TCanvas.DrawText (x, y : integer; Text, Font : string;
  FontSize: integer; Col: LongWord);
begin
  DrawText (x, y, text, Font, FontSize, Col, 255);
end;

function TCanvas.GetFont (byFont : string; Load : boolean) : TFontInfo;
var
  i : integer;
  f : TFilestream;
begin
  Result := nil;
  for i :=  0 to Fonts.Count - 1 do
    begin
      if TFontInfo (Fonts[i]).FileName = byFont then
        begin
          Result := TFontInfo (Fonts[i]);
          break;
        end;
    end;
  if (Result = nil) and FileExists (byFont) then
    begin
      Result := TFontInfo.Create;
      if FontInfo (byFont, Result) then
        Fonts.Add (Result)
      else
        begin
          Result.Free;
          Result := nil;
        end;
    end;
  if (Result = nil) or (not Load) then exit;
  if Result.Stream <> nil then exit;        // already loaded
  Result.Stream := TMemoryStream.Create;
  try
    f := TFileStream.Create (byFont, fmOpenRead);
    Result.Stream.CopyFrom (f, f.Size);
    f.Free;
  except
 //   Log ('Error loading font.');
    Result.Stream.Free;
    Result.Stream := nil;
    end;
end;

procedure TCanvas.FindFonts;
var
  i : integer;
  SearchRec : TSearchRec;
  anInfo : TFontInfo;
begin
  for i := 0 to Fonts.Count - 1 do   // first clear list
    TFontInfo (Fonts[i]).Free;
  Fonts.Clear;
  if FindFirst ('C:\*.ttf', faAnyFile, SearchRec) = 0 then
    repeat
      anInfo := TFontInfo.Create;
      if FontInfo (SearchRec.Name, anInfo) then
        Fonts.Add (anInfo)
      else
        anInfo.Free;
    until FindNext (SearchRec) <> 0;
  FindClose (SearchRec);
end;

procedure TCanvas.SetSize (w, h : integer; cf : LongWord);
var
  bc : integer;
begin
  if Buffer <> nil then FreeMem (Buffer);
  Buffer := nil;
  Width := w;
  Height := h;
  ColourFormat := cf;
  case ColourFormat of
    COLOR_FORMAT_ARGB32, {32 bits per pixel Alpha/Red/Green/Blue (ARGB8888)}
    COLOR_FORMAT_ABGR32, {32 bits per pixel Alpha/Blue/Green/Red (ABGR8888)}
    COLOR_FORMAT_RGBA32, {32 bits per pixel Red/Green/Blue/Alpha (RGBA8888)}
    COLOR_FORMAT_BGRA32 : bc := 4; {32 bits per pixel Blue/Green/Red/Alpha (BGRA8888)}
    COLOR_FORMAT_RGB24, {24 bits per pixel Red/Green/Blue (RGB888)}
    COLOR_FORMAT_BGR24  : bc := 3; {24 bits per pixel Blue/Green/Red (BGR888)}
    // COLOR_FORMAT_RGB18  = 6; {18 bits per pixel Red/Green/Blue (RGB666)}
    COLOR_FORMAT_RGB16, {16 bits per pixel Red/Green/Blue (RGB565)}
    COLOR_FORMAT_RGB15  : bc := 2; {15 bits per pixel Red/Green/Blue (RGB555)}
    COLOR_FORMAT_RGB8   : bc := 1; {8 bits per pixel Red/Green/Blue (RGB332)}
    else bc := 0;
    end;
  BufferSize := Width * Height * bc;
  if BufferSize > 0 then
    begin
      GetMem (Buffer, BufferSize);
      FillChar (Buffer^, BufferSize, 0);
    end;
end;

procedure TCanvas.Fill (Col: LongWord);
var
  Rect : Ultibo.TRect;
begin
  Rect := SetRect (0, 0, Width - 1, Height - 1);
  Fill (Rect, Col);
end;

procedure TCanvas.Fill (Rect: Ultibo.TRect; Col: LongWord);
var
  i, j : integer;
  p : pointer;
begin
  case ColourFormat of
    COLOR_FORMAT_ARGB32 : {32 bits per pixel Red/Green/Blue/Alpha (RGBA8888)}
      begin
//        Log ('Fill Width ' + IntToStr (Rect.right - rect.left) + ' Height '
//        + IntToStr (Rect.bottom - rect.top));
        if Rect.left < 0 then Rect.left:= 0;
        if Rect.top < 0 then rect.top := 0;
        if Rect.left >= Width then exit;
        if Rect.top >= Height then exit;
        if Rect.right >= Width then Rect.right := width - 1;
        if Rect.bottom >= Height then Rect.bottom := height - 1;
        if Rect.left >= Rect.right then exit;
        if Rect.top >= Rect.bottom then exit;
        for j := Rect.top to Rect.bottom do
          begin
            cardinal (p) := cardinal (Buffer) + ((j * Width) + Rect.left) * 4;
            for i := Rect.left to Rect.right do
              begin       // 000000ff blue   0000ff00 green    00ff0000 red
                PCardinal (p)^ := Col;
                Inc (p, 4);
              end;
          end;
      end;
    end;
end;

procedure TCanvas.Flush (FrameBuff : PFrameBufferDevice; x, y : integer);
begin
  FramebufferDevicePutRect (FrameBuff, x, y, Buffer, Width, Height, 0, FRAMEBUFFER_TRANSFER_DMA);
end;

procedure TCanvas.Flush (FrameBuff: PFrameBufferDevice);
begin
  FramebufferDevicePutRect (FrameBuff, Left, Top, Buffer, Width, Height, 0, FRAMEBUFFER_TRANSFER_DMA);
end;

procedure TCanvas.Assign (anOther: TCanvas);
begin
  if (anOther.Width <> Width) or (anOther.Height <> Height) or (anOther.ColourFormat <> ColourFormat) then
    SetSize (anOther.Width, anOther.Height, anOther.ColourFormat);
  Move (anOther.Buffer[0], Buffer[0], BufferSize);
end;

procedure TCanvas.DrawImage (anImage: TFPCustomImage; x, y: integer);     // derived from FPCanvas
var
  xx, xi, yi, xm, ym, r, t : integer;
  c : cardinal;
  aCol : TFPColor;
  a : byte;
  p : pointer;
begin
  xm := x + anImage.width - 1;
  if xm >= width then xm := width - 1;
  ym := y + anImage.height - 1;
  if ym >= height then ym := height - 1;
  xi := x;
  yi := y;
  for r := xi to xm do
    begin
      xx := r - x;
      for t := yi to ym do
        begin
   //     colors [r,t] := anImage.colors[xx,t-y];
          aCol := anImage.Colors[xx, t - y];
          LongWord (p) := LongWord (Buffer) + ((t * Width) + r) * 4;
          a := aCol.alpha div $100;
          c := ((GetRValue (PLongWord (p)^) * (255 - a)) + (aCol.red div $100) * a) div $100;
          c := c shl 8;
          c := c + ((GetGValue (PLongWord (p)^) * (255 - a)) + (aCol.green div $100) * a) div $100;
          c := c shl 8;
          c := c + ((GetBValue (PLongWord (p)^) * (255 - a)) + (aCol.blue div $100) *  a) div $100;
          PLongWord (p)^ := c;
        end;
    end;
end;

procedure TCanvas.DrawImage (anImage: TFPCustomImage; x, y, h, w : integer);
begin
  if IP = nil then IP := TMitchelInterpolation.Create;
   try
     with IP do
       begin
         Initialise (anImage, Self);
         Execute (x, y, h, w);
       end;
   finally
     end;
end;


constructor TCanvas.Create;
var
  res : integer;
begin
  Width := 0;
  Height := 0;
  Left := 0;
  Top := 0;
  Buffer := nil;
  IP := nil;
  ColourFormat := COLOR_FORMAT_UNKNOWN;
  Fonts := TList.Create;
  if FTLib = nil then
    begin
      res := FT_Init_FreeType (FTLib);
      if res <> 0 then log ('FTLib failed to Initialise.');
    end;
end;

destructor TCanvas.Destroy;
var
  i : integer;
begin
  for i := 0 to Fonts.Count - 1 do TFontInfo (Fonts[i]).Free;
  Fonts.Free;
  if IP <> nil then IP.Free;
  if Buffer <> nil then FreeMem (Buffer);
  inherited;
end;


{ TFPCustomInterpolation }

procedure TFPCustomInterpolation.Initialise (anImage: TFPCustomImage; aCanvas: TCanvas);
begin
  FImage := anImage;
  FCanvas := aCanvas;
end;

{ TFPBaseInterpolation }

procedure TFPBaseInterpolation.CreatePixelWeights(OldSize, NewSize : integer;
  out Entries : Pointer; out EntrySize : integer; out Support : integer);
// create an array of #NewSize entries. Each entry starts with an integer
// for the StartIndex, followed by #Support singles for the pixel weights.
// The sum of weights for each entry is 1.
var
  Entry: Pointer;

  procedure SetSupport (NewSupport : integer);
  begin
    Support := NewSupport;
    EntrySize := SizeOf (integer) + SizeOf (Single) * Support;
    GetMem (Entries, EntrySize * NewSize);
    Entry := Entries;
  end;

var
  i : Integer;
  Factor : double;
  StartPos : Double;
  StartIndex : Integer;
  j : Integer;
  FirstValue : Double;
begin
  if NewSize = OldSize then
    begin
      SetSupport (1);
      for i := 0 to NewSize - 1 do
        begin // 1:1
          PInteger (Entry)^ := i;
          inc (Entry, SizeOf (Integer));
          PSingle (Entry)^ := 1.0;
          inc (Entry, SizeOf (Single));
        end;
    end
  else if NewSize < OldSize then
    begin // shrink
      SetSupport (Max (2, (OldSize + NewSize - 1) div NewSize));
      Factor := double (OldSize) / double (NewSize);
      for i := 0 to NewSize - 1 do
        begin
          StartPos := Factor * i;
          StartIndex := Floor (StartPos);
          PInteger (Entry)^:=StartIndex;
          inc (Entry, SizeOf (Integer));
          // first pixel
          FirstValue := (1.0 - (StartPos - double (StartIndex)));
          PSingle (Entry)^ := FirstValue / Factor;
          inc (Entry, SizeOf (Single));
          // middle pixel
          for j := 1 to Support - 2 do
            begin
              PSingle (Entry)^ := 1.0 / Factor;
              inc (Entry, SizeOf (Single));
            end;
          // last pixel
          PSingle(Entry)^ := (Factor - FirstValue - (Support - 2)) / Factor;
          inc (Entry, SizeOf (Single));
        end;
      end
    else
      begin // enlarge
        if OldSize = 1 then
          begin
            SetSupport (1);
            for i := 0 to NewSize - 1 do
              begin
                // nothing to interpolate
                PInteger (Entry)^ :=0;
                inc (Entry, SizeOf (Integer));
                PSingle (Entry)^ := 1.0;
                inc (Entry, SizeOf (Single));
              end;
          end
        else
          begin
            SetSupport (2);
            Factor := double (OldSize - 1) / double (NewSize);
            for i := 0 to NewSize - 1 do
              begin
                StartPos := Factor * i + Factor / 2;
                StartIndex := Floor (StartPos);
                PInteger (Entry)^ := StartIndex;
                inc (Entry, SizeOf (Integer));
                // first pixel
                FirstValue := (1.0 - (StartPos - double (StartIndex)));
                // convert linear distribution
                FirstValue := Min (1.0, Max (0.0, Filter (FirstValue / MaxSupport)));
                PSingle(Entry)^ := FirstValue;
                inc (Entry, SizeOf (Single));
                // last pixel
                PSingle (Entry)^ := 1.0 - FirstValue;
                inc (Entry, SizeOf (Single));
              end;
          end;
     end;
  if Entry <> Entries + EntrySize * NewSize then
    raise Exception.Create ('TFPBase2Interpolation.Execute inconsistency');
end;

procedure TFPBaseInterpolation.Execute (x, y, w, h : integer);
// paint Image on Canvas at x,y,w*h
var
  dy : Integer;
  dx : Integer;
  HorzResized : PFPColor;
  xEntries : Pointer;
  xEntrySize : integer;
  xSupport : integer;// how many horizontal pixel are needed to create one pixel
  yEntries : Pointer;
  yEntrySize : integer;
  ySupport : integer;// how many vertizontal pixel are needed to create one pixel
  NewSupportLines : LongInt;
  yEntry : Pointer;
  SrcStartY : LongInt;
  LastSrcStartY : LongInt;
  LastyEntry : Pointer;
  sy : Integer;
  xEntry : Pointer;
  sx : LongInt;
  cx : Integer;
  f: Single;
  NewCol: TFPColor;
  Col: TFPColor;
  CurEntry : Pointer;
  p : pointer;
  c : cardinal;
  a : byte;
begin
  if (w <= 0) or (h <= 0) or (image.Width = 0) or (image.Height = 0) then exit;

  xEntries := nil;
  yEntries := nil;
  HorzResized := nil;
  try
    CreatePixelWeights (image.Width, w, xEntries, xEntrySize, xSupport);
    CreatePixelWeights (image.Height, h, yEntries, yEntrySize, ySupport);
    // create temporary buffer for the horizontally resized pixel for the
    // current y line
    GetMem (HorzResized, w * ySupport * SizeOf (TFPColor));
    LastyEntry := nil;
    SrcStartY := 0;
    for dy := 0 to h - 1 do
    begin
      if dy = 0 then
        begin
          yEntry := yEntries;
          SrcStartY := PInteger (yEntry)^;
          NewSupportLines := ySupport;
        end
      else
        begin
          LastyEntry := yEntry;
          LastSrcStartY := SrcStartY;
          inc (yEntry, yEntrySize);
          SrcStartY := PInteger (yEntry)^;
          NewSupportLines := SrcStartY - LastSrcStartY;
          // move lines up
          if (NewSupportLines > 0) and (ySupport > NewSupportLines) then
            System.Move (HorzResized[NewSupportLines * w],
                         HorzResized[0],
                        (ySupport - NewSupportLines) * w * SizeOf (TFPColor));
        end;

        // compute new horizontally resized line(s)
        for sy := ySupport - NewSupportLines to ySupport - 1 do
          begin
            xEntry := xEntries;
            for dx := 0 to w - 1 do
              begin
                sx := PInteger (xEntry)^;
                inc (xEntry,SizeOf(integer));
                NewCol := colTransparent;
                for cx := 0 to xSupport - 1 do
                  begin
                    f := PSingle (xEntry)^;
                    inc (xEntry, SizeOf (Single));
                    Col := image.Colors[sx + cx, SrcStartY + sy];
                    NewCol.red := Min (NewCol.red + round (Col.red * f), $ffff);
                    NewCol.green := Min (NewCol.green + round (Col.green * f), $ffff);
                    NewCol.blue := Min (NewCol.blue + round (Col.blue * f), $ffff);
                    NewCol.alpha := Min (NewCol.alpha + round (Col.alpha * f), $ffff);
                  end;
                HorzResized [dx + sy * w] := NewCol;
              end;
          end;

      // compute new vertically resized line
      for dx := 0 to w - 1 do
        begin
          CurEntry := yEntry + SizeOf (integer);
          NewCol := colTransparent;
          for sy := 0 to ySupport - 1 do
            begin
              f := PSingle (CurEntry)^;
              inc (CurEntry, SizeOf (Single));
              Col := HorzResized[dx + sy * w];
              NewCol.red := Min (NewCol.red + round (Col.red * f),$ffff);
              NewCol.green := Min (NewCol.green + round (Col.green * f),$ffff);
              NewCol.blue := Min (NewCol.blue + round (Col.blue * f), $ffff);
              NewCol.alpha := Min (NewCol.alpha + round (Col.alpha * f), $ffff);
            end;
          LongWord (p) := LongWord (Canvas.Buffer) + (((y + dy) * Canvas.Width) + x + dx) * 4;
          a := NewCol.alpha div $100;
          c := ((GetRValue (PLongWord (p)^) * (255 - a)) + (NewCol.red div $100) * a) div $100;
          c := c shl 8;
          c := c + ((GetGValue (PLongWord (p)^) * (255 - a)) + (NewCol.green div $100) * a) div $100;
          c := c shl 8;
          c := c + ((GetBValue (PLongWord (p)^) * (255 - a)) + (NewCol.blue div $100) *  a) div $100;
         PLongWord (p)^ := c;
        end;
    end;
  finally
    if xEntries <> nil then FreeMem (xEntries);
    if yEntries <> nil then FreeMem (yEntries);
    if HorzResized <> nil then FreeMem (HorzResized);
  end;
end;

function TFPBaseInterpolation.Filter (x : double) : double;
begin
  Result := x;
end;

function TFPBaseInterpolation.MaxSupport : double;
begin
  Result := 1.0;
end;

{ TMitchelInterpolation }

function TMitchelInterpolation.Filter (x : double) : double;
const
  B  = (1.0 / 3.0);
  C  = (1.0 / 3.0);
  P0 = ((  6.0 -  2.0 * B       ) / 6.0);
  P2 = ((-18.0 + 12.0 * B + 6.0 * C) / 6.0);
  P3 = (( 12.0 -  9.0 * B - 6.0 * C) / 6.0);
  Q0 = ((         8.0 * B + 24.0 * C) / 6.0);
  Q1 = ((       -12.0 * B - 48.0 * C) / 6.0);
  Q2 = ((         6.0 * B + 30.0 * C) / 6.0);
  Q3 = ((       - 1.0 * B - 6.0 * C) / 6.0);
begin
  if (x < -2.0) then
    Result := 0.0
  else if (x < -1.0) then
    Result := Q0 - x * (Q1 - x * (Q2 - x * Q3))
  else if (x < 0.0) then
    Result := P0 + x * x * (P2 - x * P3)
  else if (x < 1.0) then
    Result := P0 + x * x * (P2 + x * P3)
  else if (x < 2.0) then
    Result := Q0 + x * (Q1 + x * (Q2 + x * Q3))
  else
    Result := 0.0;
end;

function TMitchelInterpolation.MaxSupport : double;
begin
  Result := 2.0;
end;


end.

