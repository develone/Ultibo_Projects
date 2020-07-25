unit uPng;

{$mode objfpc}{$H+}
{$inline on}

{ Animated PNG unit fro ultibo  }
{ pjde 2016-7 }

{ This unit was adapted from :-                               }

{Portable Network Graphics Delphi 1.5       (29 June 2005)    }
{     gubadaud@terra.com.br                                   }
{     pngdelphi.sourceforge.net                               }
{     Gustavo Huffenbacher Daud                               }

interface

uses
  Ultibo, Classes, SysUtils, uZLib, GlobalConst, FrameBuffer, uCanvas;

const
  {animation}
  APNG_DISPOSE_OP_NONE        = 0;  // no disposal is done on this frame before rendering the next; the contents of the output buffer are left as is.
  APNG_DISPOSE_OP_BACKGROUND  = 1;  // the frame's region of the output buffer is to be cleared to fully transparent black before rendering the next frame.
  APNG_DISPOSE_OP_PREVIOUS    = 2;  // the frame's region of the output buffer is to be reverted to the previous contents before rendering the next frame.

  APNG_BLEND_OP_SOURCE        = 0;   // all color components of the frame, including alpha, overwrite the current contents of the frame's output buffer region.
  APNG_BLEND_OP_OVER          = 1;   // the frame should be composited onto the output buffer based on its alpha, using a simple OVER operation as described in the "Alpha Channel Processing" section of the PNG specification

  dirStop                     = 0;
  dirUP                       = 1;
  dirDown                     = -1;
  dirUP2                      = 2;
  dirDown2                    = -2;
  dirUP3                      = 3;
  dirDown3                    = -3;
  dirUP4                      = 4;
  dirDown4                    = -4;

  ft : array [boolean] of string = ('false', 'true');

  {ZLIB constants}
  ZLIBErrors: Array [-6..2] of string = ('incompatible version (-6)',
                                        'buffer error (-5)',
                                        'insufficient memory (-4)',
                                        'data error (-3)',
                                        'stream error (-2)',
                                        'file error (-1)',
                                        '(0)',
                                        'stream end (1)',
                                        'need dictionary (2)');
  Z_NO_FLUSH                  = 0;
  Z_FINISH                    = 4;
  Z_STREAM_END                = 1;

  {Avaliable PNG filters for mode 0}
  FILTER_NONE                 = 0;
  FILTER_SUB                  = 1;
  FILTER_UP                   = 2;
  FILTER_AVERAGE              = 3;
  FILTER_PAETH                = 4;

  {Avaliable color modes for PNG}
  COLOR_GRAYSCALE             = 0;
  COLOR_RGB                   = 2;
  COLOR_PALETTE               = 3;
  COLOR_GRAYSCALEALPHA        = 4;
  COLOR_RGBALPHA              = 6;

type
  TPNGTransparencyMode = (ptmNone, ptmBit, ptmPartial);
  PCardinal = ^Cardinal;

  TTriple = packed record
    a, b, c : Byte;
  end;
  PTriple = ^TTriple;

  TQuad = packed record
    a, b, c, d : Byte;
  end;
  PQuad = ^TQuad;

  {Pointer to array types}
  TByteArray = Array [Word] of Byte;
  PByteArray = ^TByteArray;

  TTripleArray = Array [Word] of TTriple;
  PTripleArray = ^TTripleArray;

  TQuadArray = Array [Word] of TQuad;
  PQuadArray = ^TQuadArray;

  {Forward}
  TPng = class;
  PPointerArray = ^TPointerArray;
  TPointerArray = array [Word] of pointer;

  {Chunk name object}
  TChunkName = array [0..3] of Char;

  {ZLIB Decompression extra information}
  TZStreamRec2 = packed record
    ZLIB : TZStreamRec;
    Data : pointer;
    FStream : TStream;
  end;

  PPalEntry = ^TPalEntry;
  TPalEntry = record
    r, g, b: Byte;
  end;

 // TColor = cardinal;

  TRenderedFrame = class
  private
 //   FGamma : integer;
    function GetPixels (const aX, aY: integer): Cardinal;
    procedure SetPixels (const aX, aY: integer; const Value: Cardinal);
  public
    FrameNos : integer;
    FOwner : TPng;
    Modified : boolean;
    ImageData : pointer;
    ImageAlpha : pointer;
    SourceHeight, SourceWidth : integer;
    Texts : TStringList;   // text
    Width, Height, X, Y : integer;
    Delay : integer;
    Disposal, Blend : byte;
    Rendered : boolean;
    Raw : TMemoryStream;
    RawColorType : byte;
    RawBitDepth : byte;
    function GetAlphaScanline (const LineIndex: integer): PByteArray;
    function GetScanline (const LineIndex: integer): Pointer;
    constructor Create (anOwner : TPng);
    destructor Destroy; override;
    property Pixels[const aX, aY: integer]: Cardinal read GetPixels write SetPixels;
  end;

  {IHDR data}
  pIHDRData = ^TIHDRData;
  TIHDRData = packed record
    Width, Height: integer;
    BitDepth,
    ColorType,
    CompressionMethod,
    FilterMethod,
    InterlaceMethod: Byte;
  end;


//  TPersistent = TObject;
  TInterlaceMethod = (imNone, imAdam7);
  TCompressionLevel = 0..9;

  TFilter = (pfNone, pfSub, pfUp, pfAverage, pfPaeth);
  TFilters = set of TFilter;

  { TPng }

  TPng = class
  protected
    InverseGamma: array [Byte] of Byte;
    LoadFrame : TRenderedFrame;
    FRow_Bytes, fOffset : Cardinal;
    FRow_Buffer: array [Boolean] of PByteArray;
    FRowUsed: Boolean;
    FEndPos: integer;

    procedure FilterRow;
    procedure InitializeGamma;

    function IDATZlibRead (var ZLIBStream: TZStreamRec2; Buffer: Pointer; Count : integer): integer;
    procedure DecodeInterlacedAdam7 (Stream: TStream; var ZLIBStream: TZStreamRec2);
    procedure DecodeNonInterlaced (Stream: TStream; var ZLIBStream: TZStreamRec2);

    procedure CopyNonInterlacedRGB8 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedRGB16 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedPalette8 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedPalette2 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedGray2 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedGrayscale16 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedRGBAlpha8 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedRGBAlpha16 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedGrayscaleAlpha8 (Src, Dest, Trans: PChar);
    procedure CopyNonInterlacedGrayscaleAlpha16 (Src, Dest, Trans: PChar);

    procedure CopyInterlacedRGB8 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedRGB16 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedPalette8 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedPalette2 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedGray2 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedGrayscale16 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedRGBAlpha8 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedRGBAlpha16 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedGrayscaleAlpha8 (const Pass: Byte; Src, Dest, Trans: PChar);
    procedure CopyInterlacedGrayscaleAlpha16 (const Pass: Byte; Src, Dest, Trans: PChar);

 private
    FFilters: TFilters;
    FCompressionLevel: TCompressionLevel;
    FMaxIdatSize: integer;
    FInterlaceMethod: TInterlaceMethod;
    FFrameCounter : integer;
    RenderHeight, RenderWidth : integer; // size of rendered image
    RenderData : pointer;
    RenderAlpha : pointer;
    RenderBPR : integer;      // bytes per row

    procedure ClearRenderedFrames;
    procedure GetPixelInfo (aWidth : integer; var LineSize, Offset: Cardinal);
    procedure SetMaxIdatSize (const Value: integer);
    function GetTransparencyMode: TPNGTransparencyMode;
  protected

    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetWidth (Value: integer);
    procedure SetHeight (Value: integer);
    procedure AssignPNG (Source: TPng);
    procedure DrawPartialTrans (Canvas : TCanvas; Rect : Ultibo.TRect; aFrame : TRenderedFrame; anAlpha : byte);
  public
    GammaTable : array[Byte] of Byte;
    PaletteValues : array of TQuad;
    PaletteCount : integer;

    FAnimated : boolean;
    FNosFrames,
    FNosRepeats : integer;

    SourceIHDR : TIHDRData;
    HasPalette: Boolean;
    BytesPerRow: cardinal;

    RenderedFrames : TList;
    CurrRenderedFrame : TRenderedFrame;   // this is current rendered frame

    procedure CreateAlpha (aFrame : TRenderedFrame);
    property TransparencyMode: TPNGTransparencyMode read GetTransparencyMode;

    procedure Assign (Source: TObject);
    procedure AssignTo (Dest: TObject);
    procedure Draw (aCanvas: TCanvas; const Rect: Ultibo.TRect);  overload;
    procedure Draw (aCanvas : TCanvas; const Rect: Ultibo.TRect; aFrameNos: integer); overload;
    procedure Draw (aCanvas : TCanvas; const Rect: Ultibo.TRect; aFrameNos: integer; anAlpha : byte); overload;
 //   procedure Draw (aCanvas : TCanvas; x, y : integer; aFrameNos: integer); overload;
    function IsAnimated : boolean;
    function IsEmpty: Boolean;
    function NosFrames : integer;
    function NosRepeats: integer;
    function GetMarker (aFrameNos : integer) : string;
    function GetDelay (aFrameNos : integer) : integer;

    function NextFrame (From : integer; var Dir : integer) : integer;
    function RenderFrame (aFrameNos : integer) : TRenderedFrame;
    procedure RenderStream (Stream: TStream);
    procedure RenderAllFrames;
    procedure ClearRenderStreams;
    function HitTest (aFrameNos, x, y : integer) : boolean;

    function GetRenderedFrame (aFrameNos : integer) : TRenderedFrame;
    procedure PreparePalette;

    property Filters: TFilters read FFilters write FFilters;
    property MaxIdatSize: integer read fMaxIdatSize write SetMaxIdatSize;
    property CompressionLevel: TCompressionLevel read fCompressionLevel
      write fCompressionLevel;

    constructor Create;
    destructor Destroy; override;
    property Width: integer read SourceIHDR.Width write SourceIHDR.Width;
    property Height: integer read SourceIHDR.Height write SourceIHDR.Height;
    property BitDepth: Byte read SourceIHDR.BitDepth write SourceIHDR.BitDepth;
    property ColorType: Byte read SourceIHDR.ColorType write SourceIHDR.ColorType;
    property CompressionMethod: Byte read SourceIHDR.CompressionMethod
      write SourceIHDR.CompressionMethod;
    property FilterMethod: Byte read SourceIHDR.FilterMethod
      write SourceIHDR.FilterMethod;
    property InterlaceMethod: Byte read SourceIHDR.InterlaceMethod
      write SourceIHDR.InterlaceMethod;
    function LoadFromFile (const Filename: String) : boolean;
    function LoadFromStream (Stream: TStream) : boolean;
  end;

{Calculates crc}
//function update_crc (crc: Cardinal; buf: PByteArray; len: integer): Cardinal;
function ByteSwap (const a: cardinal): cardinal;

function GetInt (p : pointer; anOffset : integer) : integer;
function GetShort (p : pointer; anOffset : integer) : word;
function GetByte (p : pointer; anOffset : integer) : byte;
function GetData (p : pointer; anOffset : integer) : pointer;

implementation

uses uLog;

function ColourDepth (aFormat : LongWord) : byte;
begin
  Result := 0;
  case aFormat of
    COLOR_FORMAT_ARGB32,               {32 bits per pixel Alpha/Red/Green/Blue (ARGB8888)}
    COLOR_FORMAT_ABGR32,               {32 bits per pixel Alpha/Blue/Green/Red (ABGR8888)}
    COLOR_FORMAT_RGBA32,               {32 bits per pixel Red/Green/Blue/Alpha (RGBA8888)}
    COLOR_FORMAT_BGRA32 : Result := 4; {32 bits per pixel Blue/Green/Red/Alpha (BGRA8888)}
    COLOR_FORMAT_RGB24,                {24 bits per pixel Red/Green/Blue (RGB888)}
    COLOR_FORMAT_BGR24  : Result := 3; {24 bits per pixel Blue/Green/Red (BGR888)}
 //  COLOR_FORMAT_RGB18  = 6; {18 bits per pixel Red/Green/Blue (RGB666)}
    COLOR_FORMAT_RGB16,                {16 bits per pixel Red/Green/Blue (RGB565)}
    COLOR_FORMAT_RGB15  : Result := 2; {15 bits per pixel Red/Green/Blue (RGB555)}
    COLOR_FORMAT_RGB8   : Result := 1; {8 bits per pixel Red/Green/Blue (RGB332)}
  end;
end;

// from delphi windows module
procedure CopyMemory (Destination: Pointer; Source: Pointer; Length: DWORD);
begin
  Move (Source^, Destination^, Length);
end;

procedure ZeroMemory (Destination: Pointer; Length: DWORD);
begin
  FillChar (Destination^, Length, 0);
end;

function MulDiv(nNumber, nNumerator, nDenominator: integer): integer;
var
  x : int64;
begin
  x := nNumber * nNumerator;
  Result := x div nDenominator;
end;

{Calculates the paeth predictor}
function PaethPredictor (a, b, c: Byte): Byte;
var
  pa, pb, pc: integer;
begin
  { a = left, b = above, c = upper left }
  pa := abs (b - c);      { distances to a, b, c }
  pb := abs (a - c);
  pc := abs (a + b - c * 2);
  { return nearest of a, b, c, breaking ties in order a, b, c }
  if (pa <= pb) and (pa <= pc) then
    Result := a
  else
    if pb <= pc then
      Result := b
    else
      Result := c;
end;

function ByteSwap (const a : cardinal): cardinal; inline;
begin
  Result := ((a and $ff) shl 24) + ((a and $ff00) shl 8) +
            ((a and $ff0000) shr 8) + ((a and $ff000000) shr 24);
end;

function ByteSwap16 (w : Word): Word; inline;
begin
  Result := ((w and $ff) shl 8) + ((w and $ff00) shr 8);
end;

function GetInt (p : pointer; anOffset : integer) : integer;
begin
  Result := ByteSwap (pinteger (Cardinal (p) + anOffset)^);
end;

function GetShort (p : pointer; anOffset : integer) : word;
begin
  Result := ByteSwap16 (pWord (Cardinal (p) + anOffset)^);
end;

function GetByte (p : pointer; anOffset : integer) : byte;
begin
  Result := PByte (Cardinal (p) + anOffset)^;
end;

function GetData (p : pointer; anOffset : integer) : pointer;
begin
  Result := pointer (Cardinal (p) + anOffset);
end;

{Calculates number of bytes for the number of pixels using the}
{color mode in the paramenter}
function BytesForPixels (const Pixels: integer; const ColorType,
  BitDepth: Byte): integer;
begin
  case ColorType of
    {Palette and grayscale contains a single value, for palette}
    {an value of size 2^bitdepth pointing to the palette index}
    {and grayscale the value from 0 to 2^bitdepth with color intesity}
    COLOR_GRAYSCALE, COLOR_PALETTE : Result := (Pixels * BitDepth + 7) div 8;
    {RGB contains 3 values R, G, B with size 2^bitdepth each}
    COLOR_RGB : Result := (Pixels * BitDepth * 3) div 8;
    {Contains one value followed by alpha value booth size 2^bitdepth}
    COLOR_GRAYSCALEALPHA : Result := (Pixels * BitDepth * 2) div 8;
    {Contains four values size 2^bitdepth, Red, Green, Blue and alpha}
    COLOR_RGBALPHA : Result := (Pixels * BitDepth * 4) div 8;
    else
      Result := 0;
  end; {case ColorType}
end;

{ZLIB support}
const
  ZLIBAllocate = High (Word);
{Initializes ZLIB for decompression}
function ZLIBInitInflate (Stream: TStream): TZStreamRec2;
begin
  {Fill record}
  FillChar (Result, SIZEOF (TZStreamRec2), #0);
  {Set internal record information}
  with Result do
    begin
      GetMem (Data, ZLIBAllocate);
      FStream := Stream;
    end;
  {Init decompression}
  InflateInit_ (Result.ZLIB, zlib_version, SizeOf (TZStreamRec));
end;

{Initializes ZLIB for compression}
function ZLIBInitDeflate (Stream: TStream; Level: TCompressionlevel; Size: Cardinal): TZStreamRec2;
begin
  {Fill record}
  FillChar (Result, SizeOf (TZStreamRec2), #0);
  {Set internal record information}
  with Result, ZLIB do
    begin
      GetMem (Data, Size);
      fStream := Stream;
      next_out := Data;
      avail_out := Size;
    end;
  {Inits compression}
  deflateInit_ (Result.zlib, Level, zlib_version, SizeOf (TZStreamRec));
end;

{Terminates ZLIB for compression}
procedure ZLIBTerminateDeflate (var ZLIBStream: TZStreamRec2);
begin
  {Terminates decompression}
  DeflateEnd (ZLIBStream.zlib);
  {Free internal record}
  FreeMem (ZLIBStream.Data, ZLIBAllocate);
end;

{Terminates ZLIB for decompression}
procedure ZLIBTerminateInflate (var ZLIBStream: TZStreamRec2);
begin
  {Terminates decompression}
  InflateEnd(ZLIBStream.zlib);
  {Free internal record}
  FreeMem (ZLIBStream.Data, ZLIBAllocate);
end;

{Decompresses ZLIB into a memory address}
function DecompressZLIB (const Input: Pointer; InputSize: integer;
  var Output: Pointer; var OutputSize: integer;
  var ErrorOutput: String): Boolean;
var
  StreamRec : TZStreamRec;
  Buffer    : Array[Byte] of Byte;
  InflateRet: integer;
begin
  with StreamRec do
    begin
      {Initializes}
      Result := True;
      OutputSize := 0;
      {Prepares the data to decompress}
      FillChar (StreamRec, SizeOf (TZStreamRec), #0);
      InflateInit_ (StreamRec, zlib_version, SizeOf (TZStreamRec));
      next_in := Input;
      avail_in := InputSize;
      {Decodes data}
      repeat
        {In case it needs an output buffer}
        if (avail_out = 0) then
          begin
            next_out := @Buffer;
            avail_out := SizeOf (Buffer);
          end; {if (avail_out = 0)}
        {Decompress and put in output}
        InflateRet := inflate (StreamRec, 0);
        if (InflateRet = Z_STREAM_END) or (InflateRet = 0) then
          begin
            {Reallocates output buffer}
            inc (OutputSize, total_out);
            if Output = nil then
              GetMem (Output, OutputSize) else ReallocMem (Output, OutputSize);
            {Copies the new data}
       //     CopyMemory (Ptr (Longint (Output) + OutputSize - total_out),
         //     @Buffer, total_out);    reinstate
          end {if (InflateRet = Z_STREAM_END) or (InflateRet = 0)}
        {Now tests for errors}
        else if InflateRet < 0 then
          begin
            Result := False;
            ErrorOutput := StreamRec.msg;
            InflateEnd (StreamRec);
            Exit;
          end; {if InflateRet < 0}
      until InflateRet = Z_STREAM_END;
      {Terminates decompression}
      InflateEnd (StreamRec);
    end; {with StreamRec}
end;

{Compresses ZLIB into a memory address}
function CompressZLIB (Input: Pointer; InputSize, CompressionLevel: integer;
  var Output: Pointer; var OutputSize: integer;
  var ErrorOutput: String): Boolean;
var
  StreamRec : TZStreamRec;
  Buffer    : Array[Byte] of Byte;
  DeflateRet: integer;
begin
  with StreamRec do
    begin
      Result := True; {By default returns TRUE as everything might have gone ok}
      OutputSize := 0; {Initialize}
      {Prepares the data to compress}
      FillChar (StreamRec, SizeOf (TZStreamRec), #0);
      DeflateInit_ (StreamRec, CompressionLevel,zlib_version, SizeOf (TZStreamRec));
      next_in := Input;
      avail_in := InputSize;
      while avail_in > 0 do
        begin
          {When it needs new buffer to stores the compressed data}
          if avail_out = 0 then
            begin
              {Restore buffer}
              next_out := @Buffer;
              avail_out := SizeOf(Buffer);
            end; {if avail_out = 0}
          {Compresses}
          DeflateRet := deflate(StreamRec, Z_FINISH);
          if (DeflateRet = Z_STREAM_END) or (DeflateRet = 0) then
            begin
              {Updates the output memory}
              inc(OutputSize, total_out);
              if Output = nil then
                GetMem (Output, OutputSize) else ReallocMem (Output, OutputSize);
              {Copies the new data}
         //     CopyMemory(Ptr(Longint(Output) + OutputSize - total_out),
         //       @Buffer, total_out);     reinstate
            end {if (InflateRet = Z_STREAM_END) or (InflateRet = 0)}
          {Now tests for errors}
          else if DeflateRet < 0 then
            begin
              Result := False;
              ErrorOutput := StreamRec.msg;
              DeflateEnd (StreamRec);
              Exit;
            end; {if InflateRet < 0}
        end; {while avail_in > 0}
    {Finishes compressing}
    DeflateEnd (StreamRec);
  end; {with StreamRec}
end;

{Prepares the image palette}
procedure TPng.PreparePalette;
var
  Entries: Word;
  j      : integer;
begin      // reinstate
  {In case the image uses grayscale, build a grayscale palette}
  with SourceIHDR do
    if (ColorType = COLOR_GRAYSCALE) or (ColorType = COLOR_GRAYSCALEALPHA) then
      begin
        {Calculate total number of palette entries}
        Entries := 256;         // check
        for j := 0 to Entries - 1 do
          with PaletteValues[j] do
            begin
              {Calculate each palette entry}
              a := GammaTable[MulDiv (j, 255, Entries - 1)];
              b := a;
              c := a;
              d := 255;
            end; {with BitmapInfo.bmiColors[j]}
      end; {if ColorType = COLOR_GRAYSCALE..., with Header}
end;

{Reads from ZLIB}
function TPng.IDATZlibRead (var ZLIBStream: TZStreamRec2;
  Buffer: Pointer; Count : integer) : integer;
var
  ProcResult : integer;
begin
  {Uses internal record pointed by ZLIBStream to gather information}
  with ZLIBStream, ZLIBStream.zlib do
    begin
      next_out := Buffer;
      avail_out := Count;
      while avail_out > 0 do
        begin
          if (fStream.Position = fStream.Size) and (avail_out > 0) and (avail_in = 0) then
            begin
              Log ('Missing Multiple IDAT.');
              Result := -1;
              exit;
            end;  // if more IDAT required
          if avail_in = 0 then
            begin
              if fStream.Position + ZLIBAllocate > fStream.Size then
                avail_in := fStream.Read (Data^, fStream.Size - fStream.Position)
              else
                avail_in := fStream.Read (Data^, ZLIBAllocate);
              if avail_in = 0 then
                begin
                  Result := Count - avail_out;
                  Exit;
                end;
              next_in := Data;
            end; {if avail_in = 0}
          ProcResult := inflate(zlib, 0);
          if (ProcResult < 0) then
            begin
              Result := -1;
              Log ('ZLIB Erroor ' + zliberrors[ProcResult]);
              exit;
            end;
        end; {while avail_out > 0}
    end; {with}
  Result := Count;
end;

{TChunkIDAT implementation}

const
  {Adam 7 interlacing values}
  RowStart: array [0..6] of cardinal = (0, 0, 4, 0, 2, 0, 1);
  ColumnStart: array [0..6] of cardinal = (0, 4, 0, 2, 0, 1, 0);
  RowIncrement: array [0..6] of cardinal = (8, 8, 8, 4, 4, 2, 2);
  ColumnIncrement: array [0..6] of cardinal = (8, 8, 4, 4, 2, 2, 1);

// These functions have been transferred to Png Object
{Copy interlaced images with 1 byte for R, G, B}
procedure TPng.CopyInterlacedRGB8 (const Pass: Byte; Src, Dest, Trans: PChar);
var
  Col: Cardinal;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar (Cardinal (Dest) + Col * 3);
  repeat
    {Copy this row}
    Byte (Dest^) := GammaTable[pByte (Cardinal (Src) + 2)^]; inc (Dest);
    Byte (Dest^) := GammaTable[pByte (Cardinal (Src) + 1)^]; inc (Dest);
    Byte (Dest^) := GammaTable[pByte (Cardinal (Src)    )^]; inc (Dest);
    {Move to next column}
    inc (Src, 3);
    inc (Dest, ColumnIncrement[Pass] * 3 - 3);
    inc (Col, ColumnIncrement[Pass]);
  until Col >= RenderWidth;
end;

{Copy interlaced images with 2 bytes for R, G, B}
procedure TPng.CopyInterlacedRGB16 (const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  Col: Cardinal;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar (Cardinal (Dest) + Col * 3);
  repeat
    {Copy this row}
    Byte (Dest^) := GammaTable[pByte (Cardinal (Src) + 4)^]; inc (Dest);
    Byte (Dest^) := GammaTable[pByte (Cardinal (Src) + 2)^]; inc (Dest);
    Byte (Dest^) := GammaTable[pByte (Cardinal (Src)    )^]; inc (Dest);
    {Move to next column}
    inc (Src, 6);
    inc (Dest, ColumnIncrement[Pass] * 3 - 3);
    inc (Col, ColumnIncrement[Pass]);
  until Col >= RenderWidth;
end;

{Copy ímages with palette using bit depths 1, 4 or 8}
procedure TPng.CopyInterlacedPalette8 (const Pass: Byte;
  Src, Dest, Trans: PChar);
const
  BitTable: array [1..8] of cardinal = ($1, $3, 0, $F, 0, 0, 0, $FF);
  StartBit: array [1..8] of cardinal = (7 , 0 , 0, 4,  0, 0, 0, 0);
var
  CurBit : integer;
  Col : Cardinal;
  Dest2: PChar;
begin
// this may need to be rewritten

  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := StartBit[SourceIHDR.BitDepth];
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := PChar (Cardinal (Dest) + (SourceIHDR.BitDepth * Col) div 8);
      {Copy data}
      Byte(Dest2^) := Byte(Dest2^) or
        (((Byte (Src^) shr CurBit) and BitTable[SourceIHDR.BitDepth])
          shl (StartBit[SourceIHDR.BitDepth] - (Col * SourceIHDR.BitDepth mod 8)));
      {Move to next column}
      inc (Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec (CurBit, SourceIHDR.BitDepth);
    until CurBit < 0;
    {Move to next byte in source}
    inc(Src);
  until Col >= RenderWidth;
end;

{Copy ímages with palette using bit depth 2}
procedure TPng.CopyInterlacedPalette2 (const Pass: Byte; Src, Dest,
  Trans: PChar);
var
  CurBit : integer;
  Col : Cardinal;
  Dest2: PChar;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := 6;
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := PChar (Cardinal (Dest) + Col div 2);
      {Copy data}
      Byte (Dest2^) := Byte (Dest2^) or (((Byte (Src^) shr CurBit) and $3)
         shl (4 - (4 * Col) mod 8));
      {Move to next column}
      inc (Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec(CurBit, 2);
    until CurBit < 0;
    {Move to next byte in source}
    inc (Src);
  until Col >= RenderWidth;
end;

{Copy ímages with grayscale using bit depth 2}
procedure TPng.CopyInterlacedGray2 (const Pass: Byte;
  Src, Dest, Trans: PChar);
var
  CurBit: integer;
  Col : Cardinal;
  Dest2: PChar;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  repeat
    {Copy data}
    CurBit := 6;
    repeat
      {Adjust pointer to pixel byte bounds}
      Dest2 := PChar (Cardinal (Dest) + Col div 2);
      {Copy data}
      Byte(Dest2^) := Byte (Dest2^) or ((((Byte (Src^) shr CurBit) shl 2) and $F)
         shl (4 - (Col * 4) mod 8));
      {Move to next column}
      inc (Col, ColumnIncrement[Pass]);
      {Will read next bits}
      dec (CurBit, 2);
    until CurBit < 0;
    {Move to next byte in source}
    inc (Src);
  until Col >= RenderWidth;
end;

{Copy ímages with palette using 2 bytes for each pixel}
procedure TPng.CopyInterlacedGrayscale16 (const Pass: Byte; Src, Dest, Trans: PChar);
var
  Col: Cardinal;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar (Cardinal (Dest) + Col);
  repeat
    {Copy this row}
    Dest^ := Src^; inc (Dest);
    {Move to next column}
    inc (Src, 2);
    inc (Dest, ColumnIncrement[Pass] - 1);
    inc (Col, ColumnIncrement[Pass]);
  until Col >= RenderWidth;
end;

{Decodes interlaced RGB alpha with 1 byte for each sample}
procedure TPng.CopyInterlacedRGBAlpha8 (const Pass: Byte; Src, Dest, Trans: PChar);
var
  Col: Cardinal;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar (Cardinal (Dest) + Col * 3);
  Trans := PChar (Cardinal (Trans) + Col);
  repeat
    {Copy this row and alpha value}
    Trans^ := PChar (Cardinal (Src) + 3)^;
    Byte (Dest^)  := GammaTable[pByte (Cardinal (Src) + 2)^]; inc (Dest);
    Byte (Dest^)  := GammaTable[pByte (Cardinal (Src) + 1)^]; inc (Dest);
    Byte (Dest^)  := GammaTable[pByte (Cardinal (Src)    )^]; inc (Dest);
    {Move to next column}
    inc (Src, 4);
    inc (Dest, ColumnIncrement[Pass] * 3 - 3);
    inc (Trans, ColumnIncrement[Pass]);
    inc (Col, ColumnIncrement[Pass]);
  until Col >= RenderWidth;
end;

{Decodes interlaced RGB alpha with 2 bytes for each sample}
procedure TPng.CopyInterlacedRGBAlpha16 (const Pass: Byte; Src, Dest, Trans: PChar);
var
  Col: Cardinal;
begin
  {Get first column and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar (Cardinal (Dest) + Col * 3);
  Trans := PChar (Cardinal (Trans) + Col);
  repeat
    {Copy this row and alpha value}
    Trans^ := PChar (Cardinal (Src) + 6)^;
    Byte (Dest^)  := GammaTable[pByte (Cardinal (Src) + 4)^]; inc (Dest);
    Byte (Dest^)  := GammaTable[pByte (Cardinal (Src) + 2)^]; inc (Dest);
    Byte (Dest^)  := GammaTable[pByte (Cardinal (Src)    )^]; inc (Dest);
    {Move to next column}
    inc (Src, 8);
    inc (Dest, ColumnIncrement[Pass] * 3 - 3);
    inc (Trans, ColumnIncrement[Pass]);
    inc (Col, ColumnIncrement[Pass]);
  until Col >= RenderWidth;
end;

{Decodes 8 bit grayscale image followed by an alpha sample}
procedure TPng.CopyInterlacedGrayscaleAlpha8 (const Pass: Byte; Src, Dest, Trans: PChar);
var
  Col: Cardinal;
begin
  {Get first column, pointers to the data and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar (Cardinal (Dest) + Col);
  Trans := PChar (Cardinal (Trans) + Col);
  repeat
    {Copy this grayscale value and alpha}
    Dest^ := Src^;  inc (Src);
    Trans^ := Src^; inc (Src);
    {Move to next column}
    inc (Dest, ColumnIncrement[Pass]);
    inc (Trans, ColumnIncrement[Pass]);
    inc (Col, ColumnIncrement[Pass]);
  until Col >= RenderWidth;
end;

{Decodes 16 bit grayscale image followed by an alpha sample}
procedure TPng.CopyInterlacedGrayscaleAlpha16 (const Pass: Byte; Src, Dest, Trans: PChar);
var
  Col: Cardinal;
begin
  {Get first column, pointers to the data and enter in loop}
  Col := ColumnStart[Pass];
  Dest := PChar (Cardinal (Dest) + Col);
  Trans := PChar (Cardinal (Trans) + Col);
  repeat
    {Copy this grayscale value and alpha, transforming 16 bits into 8}
    Dest^ := Src^;  inc (Src, 2);
    Trans^ := Src^; inc (Src, 2);
    {Move to next column}
    inc (Dest, ColumnIncrement[Pass]);
    inc (Trans, ColumnIncrement[Pass]);
    inc (Col, ColumnIncrement[Pass]);
  until Col >= RenderWidth;
end;

{Decodes an interlaced image}
procedure TPng.DecodeInterlacedAdam7 (Stream: TStream; var ZLIBStream: TZStreamRec2);
var
  CurrentPass: Byte;
  PixelsThisRow: integer;
  CurrentRow: LongInt;
  Trans, Data: PChar;
  CopyProc: procedure (const Pass: Byte; Src, Dest, Trans : PChar) of object;
begin
  CopyProc := nil; {Initialize}
  {Determine method to copy the image data}
  case SourceIHDR.ColorType of
    {R, G, B values for each pixel}
    COLOR_RGB:
      case SourceIHDR.BitDepth of
        8:  CopyProc := @CopyInterlacedRGB8;
       16:  CopyProc := @CopyInterlacedRGB16;
      end {case Header.BitDepth};
    {Palette}
    COLOR_PALETTE, COLOR_GRAYSCALE:
      case SourceIHDR.BitDepth of
        8 : CopyProc := @CopyInterlacedPalette8;
      end;
    {RGB followed by alpha}
    COLOR_RGBALPHA:
      case SourceIHDR.BitDepth of
        8:  CopyProc := @CopyInterlacedRGBAlpha8;
       16:  CopyProc := @CopyInterlacedRGBAlpha16;
      end;
    {Grayscale followed by alpha}
    COLOR_GRAYSCALEALPHA:
      case SourceIHDR.BitDepth of
        8:  CopyProc := @CopyInterlacedGrayscaleAlpha8;
       16:  CopyProc := @CopyInterlacedGrayscaleAlpha16;
      end;
  end {case Header.ColorType};

  if @CopyProc = nil then exit;

  {Adam7 method has 7 passes to make the final image}
  for CurrentPass := 0 to 6 do
    begin
      {Calculates the number of pixels and bytes for this pass row}
      PixelsThisRow := (RenderWidth - ColumnStart[CurrentPass] +
        ColumnIncrement[CurrentPass] - 1) div ColumnIncrement[CurrentPass];
      fRow_Bytes := BytesForPixels (PixelsThisRow, SourceIHDR.ColorType, SourceIHDR.BitDepth);
      {Clear buffer for this pass}
      ZeroMemory (FRow_Buffer[not FRowUsed], FRow_Bytes);
      {Get current row index}
      CurrentRow := RowStart[CurrentPass];
      {Get a pointer to the current row image data}      // this may need redirection
      Data := pointer (Cardinal (RenderData) + RenderBPR * (RenderHeight - 1 - CurrentRow));
      Trans := pointer (Cardinal (RenderAlpha) + RenderWidth * CurrentRow);

      if fRow_Bytes > 0 then {There must have bytes for this interlaced pass}
        while CurrentRow < RenderHeight do
          begin
            {Reads this line and filter}
            if IDATZlibRead (ZLIBStream, @FRow_Buffer[FRowUsed]^[0], FRow_Bytes + 1) = 0 then break;
            FilterRow;
            {Copy image data}
            CopyProc (CurrentPass, @FRow_Buffer[FRowUsed]^[1], Data, Trans);
            {Use the other RowBuffer item}
            fRowUsed := not FRowUsed;
            {Move to the next row}
            inc (CurrentRow, RowIncrement[CurrentPass]);
            {Move pointer to the next line}
            dec (Data, RowIncrement[CurrentPass] * RenderBPR);
            inc (Trans, RowIncrement[CurrentPass] * RenderWidth);
          end; {while CurrentRow < ImageHeight}
    end {FOR CurrentPass};
end;

{Copy 8 bits RGB image}
procedure TPng.CopyNonInterlacedRGB8 (Src, Dest, Trans: PChar);
var
  i: integer;
begin

  for i := 1 to RenderWidth do
    begin
      {Copy pixel values}
      Byte (Dest^) := GammaTable[PByte (Cardinal (Src) + 2)^]; inc (Dest);
      Byte (Dest^) := GammaTable[PByte (Cardinal (Src) + 1)^]; inc (Dest);
      Byte (Dest^) := GammaTable[PByte (Cardinal (Src)    )^]; inc (Dest);
      {Move to next pixel}
      inc (Src, 3);
    end;
end;

{Copy 16 bits RGB image}
procedure TPng.CopyNonInterlacedRGB16 (Src, Dest, Trans: PChar);
var
  i: integer;
begin
  for i := 1 to RenderWidth do
    begin
      //Since windows does not supports 2 bytes for
      //each R, G, B value, the method will read only 1 byte from it
      {Copy pixel values}
      Byte (Dest^) := GammaTable[pByte (Cardinal (Src) + 4)^]; inc (Dest);
      Byte (Dest^) := GammaTable[pByte (Cardinal (Src) + 2)^]; inc (Dest);
      Byte (Dest^) := GammaTable[pByte (Cardinal (Src)    )^]; inc (Dest);
      {Move to next pixel}
      inc (Src, 6);
    end; {for I}
end;

{Copy types using palettes (1, 4 or 8 bits per pixel)}
procedure TPng.CopyNonInterlacedPalette8 (Src, Dest, Trans: PChar);
var
  i: integer;
  anIndex : byte;    // reinstate
begin
  for i := 1 TO RenderWidth DO
    begin
      anIndex := pByte (Src)^;
      if anIndex >= PaletteCount then anIndex := 0;
      Trans^ := Char (PaletteValues[anIndex].d);
      Byte (Dest^) := PaletteValues[anIndex].a;   inc (Dest);
      Byte (Dest^) := PaletteValues[anIndex].b; inc (Dest);
      Byte (Dest^) := PaletteValues[anIndex].c;  inc (Dest);
      inc (Src); inc (Trans);
    end;
end;

{Copy grayscale types using 2 bits for each pixel}
procedure TPng.CopyNonInterlacedGray2 (Src, Dest, Trans: PChar);
var
  i: integer;
begin
  {2 bits is not supported, this routine will converted into 4 bits}
  for i := 1 to fRow_Bytes do
    begin
      Byte (Dest^) := ((Byte (Src^) shr 2) and $F) or ((Byte (Src^)) and $F0); inc (Dest);
      Byte (Dest^) := ((Byte (Src^) shl 2) and $F) or ((Byte (Src^) shl 4) and $F0); inc (Dest);
      inc (Src);
    end;
end;

{Copy types using palette with 2 bits for each pixel}
procedure TPng.CopyNonInterlacedPalette2 (Src, Dest, Trans: PChar);
var
  i: integer;
begin
  {2 bits is not supported, this routine will converted into 4 bits}
  for i := 1 to fRow_Bytes do
    begin
      Byte (Dest^) := ((Byte(Src^) shr 4) and $3) or ((Byte (Src^) shr 2) and $30); inc (Dest);
      Byte (Dest^) := (Byte(Src^) and $3) or ((Byte (Src^) shl 2) and $30); inc (Dest);
      inc (Src);
    end;
end;

{Copy grayscale images with 16 bits}
procedure TPng.CopyNonInterlacedGrayscale16 (Src, Dest, Trans: PChar);
var
  i : integer;
begin
  for i := 1 to RenderWidth do
    begin
      Dest^ := Src^; inc (Dest);
      {Move to next pixel}
      inc (Src, 2);
    end;
end;

{Copy 8 bits per sample RGB images followed by an alpha byte}
procedure TPng.CopyNonInterlacedRGBAlpha8 (Src, Dest, Trans: PChar);
var
  i: integer;
begin
 // Log ('Copy Non InterlacedRGBAlpha8. Renderwidth ' + RenderWidth.ToString);
  for i := 1 TO RenderWidth DO
    begin
      {Copy pixel values and transparency}
      Trans^ := PChar (Cardinal (Src) + 3)^;
      Byte (Dest^)  := GammaTable[pByte (Cardinal (Src) + 2)^]; inc (Dest);
      Byte (Dest^)  := GammaTable[pByte (Cardinal (Src) + 1)^]; inc (Dest);
      Byte (Dest^)  := GammaTable[pByte (Cardinal (Src)    )^]; inc (Dest);
      {Move to next pixel}
      inc (Src, 4); inc (Trans);
    end;
end;

{Copy 16 bits RGB image with alpha using 2 bytes for each sample}
procedure TPng.CopyNonInterlacedRGBAlpha16 (Src, Dest, Trans: PChar);
var
  i : integer;
begin
  for i := 1 to RenderWidth do
    begin
      //Copy rgb and alpha values (transforming from 16 bits to 8 bits)
      {Copy pixel values}
      Trans^ := PChar (Cardinal (Src) + 6)^;
      Byte (Dest^)  := GammaTable[pByte (Cardinal (Src) + 4)^]; inc (Dest);
      Byte (Dest^)  := GammaTable[pByte (Cardinal (Src) + 2)^]; inc (Dest);
      Byte (Dest^)  := GammaTable[pByte (Cardinal (Src)    )^]; inc (Dest);
      {Move to next pixel}
      inc (Src, 8); inc (Trans);
    end;
end;

{Copy 8 bits per sample grayscale followed by alpha}
procedure TPng.CopyNonInterlacedGrayscaleAlpha8 (Src, Dest, Trans: PChar);
var
  i : integer;
begin
  for i := 1 to RenderWidth do
    begin
      {Copy alpha value and then gray value}
      Dest^  := Src^; inc (Src);
      Trans^ := Src^; inc (Src);
      inc (Dest); inc (Trans);
    end;
end;

{Copy 16 bits per sample grayscale followed by alpha}
procedure TPng.CopyNonInterlacedGrayscaleAlpha16 (Src, Dest, Trans: PChar);
var
  i: integer;
begin
  for i := 1 to RenderWidth do
    begin
    {Copy alpha value and then gray value}
      Dest^  := Src^; inc (Src, 2);
      Trans^ := Src^; inc (Src, 2);
      inc (Dest); inc (Trans);
    end;
end;

{Decode non interlaced image}
procedure TPng.DecodeNonInterlaced (Stream: TStream;
  var ZLIBStream: TZStreamRec2);
var
  j: Cardinal;
  Trans, Data: PChar;
  CopyProc: procedure (Src, Dest, Trans: PChar) of object;
begin
//  Log ('Decoding Non Interlaced. ColourType ' + SourceIHDR.ColorType.ToString + ' Bit Depth ' + SourceIHDR.BitDepth.ToString);
  CopyProc := nil; {Initialize}
  {Determines the method to copy the image data}
  case SourceIHDR.ColorType of
    {R, G, B values}
    COLOR_RGB:
      case SourceIHDR.BitDepth of
        8 : CopyProc := @CopyNonInterlacedRGB8;
       16 : CopyProc := @CopyNonInterlacedRGB16;
      end;
    {Types using palettes}
    COLOR_PALETTE, COLOR_GRAYSCALE:
      case SourceIHDR.BitDepth of
        8  : CopyProc := @CopyNonInterlacedPalette8;
        16 : CopyProc := @CopyNonInterlacedGrayscale16;
      end;
    {R, G, B followed by alpha}
    COLOR_RGBALPHA:
      case SourceIHDR.BitDepth of
        8  : CopyProc := @CopyNonInterlacedRGBAlpha8;
        16 : CopyProc := @CopyNonInterlacedRGBAlpha16;
      end;
    {Grayscale followed by alpha}
    COLOR_GRAYSCALEALPHA:
      case SourceIHDR.BitDepth of
        8  : CopyProc := @CopyNonInterlacedGrayscaleAlpha8;
       16  : CopyProc := @CopyNonInterlacedGrayscaleAlpha16;
      end;
  end;
  if @CopyProc = nil then exit;

  {animation}
  Data := RenderData;
  Trans := RenderAlpha;
  {Reads each line}
  for j := 0 to RenderHeight - 1 do
    begin
      {Read this line Row_Buffer[RowUsed][0] if the filter type for this line}
      if IDATZlibRead (ZLIBStream, @FRow_Buffer[FRowUsed]^[0], FRow_Bytes + 1) = 0 then
        begin
          Log ('IDAT ZLib Read Error.');
          break;
        end;
      {Filter the current row}
      FilterRow;
      {Copies non interlaced row to image}
      CopyProc (@FRow_Buffer[FRowUsed]^[1], Data, Trans);
      {Invert line used}
      FRowUsed := not FRowUsed;
      inc (Data, RenderBPR);
      inc (Trans, RenderWidth);
    end; /// j
end;

{Filter the current line}
procedure TPng.FilterRow;
var
  pp: Byte;
  vv, left, above, aboveleft: integer;
  Col: Cardinal;
begin
  {Test the filter}

  case FRow_Buffer[fRowUsed]^[0] of
    {No filtering for this line}
    FILTER_NONE: begin end;
    {AND 255 serves only to never let the result be larger than one byte}
    {Sub filter}
    FILTER_SUB:
      for Col := fOffset + 1 to fRow_Bytes do
        FRow_Buffer[fRowUsed]^[Col] := (fRow_Buffer[fRowUsed]^[Col] +
          FRow_Buffer[fRowUsed]^[Col - fOffset]) and 255;
    {Up filter}
    FILTER_UP:
      for Col := 1 to fRow_Bytes do
        FRow_Buffer[fRowUsed]^[Col] := (fRow_Buffer[fRowUsed]^[Col] +
          fRow_Buffer[not fRowUsed]^[Col]) and 255;
    {Average filter}
    FILTER_AVERAGE:
      for Col := 1 to fRow_Bytes do
        begin
          {Obtains up and left pixels}
          above := FRow_Buffer[not FRowUsed]^[Col];
          if col - 1 < FOffset then
            left := 0
          else
            Left := FRow_Buffer[FRowUsed]^[Col - FOffset];

          {Calculates}
          FRow_Buffer[FRowUsed]^[Col] := (fRow_Buffer[FRowUsed]^[Col] +
            (left + above) div 2) and 255;
        end;
    {Paeth filter}
    FILTER_PAETH:
      begin
        {Initialize}
        left := 0;
        aboveleft := 0;
        {Test each byte}
        for Col := 1 to fRow_Bytes do
          begin
            {Obtains above pixel}
            above := fRow_Buffer[not fRowUsed]^[Col];
            {Obtains left and top-left pixels}
            if (col - 1 >= fOffset) Then
              begin
                left := fRow_buffer[fRowUsed]^[col - fOffset];
                aboveleft := fRow_buffer[not fRowUsed]^[col - fOffset];
              end;
            {Obtains current pixel and paeth predictor}
            vv := fRow_buffer[fRowUsed]^[Col];
            pp := PaethPredictor(left, above, aboveleft);
            {Calculates}
            fRow_Buffer[fRowUsed]^[Col] := (pp + vv) and $FF;
          end; {for}
      end;
    end {case};

end;

//const
//  IDATHeader: array [0..3] of char = ('I', 'D', 'A', 'T');
 // BUFFER = 5;

{Writes the IDAT using the settings}
(*
procedure WriteIDAT (Stream: TStream; Data: Pointer; const Length: Cardinal);
var
  ChunkLen, CRC: Cardinal;
begin
  Stream.Write (Data^, Length);
  exit;
  {Writes IDAT header}
  ChunkLen := ByteSwap(Length);
  Stream.Write (ChunkLen, 4);                      {Chunk length}
  Stream.Write (IDATHeader[0], 4);                 {Idat header}
  CRC := update_crc ($ffffffff, @IDATHeader[0], 4); {Crc part for header}
  {Writes IDAT data and calculates CRC for data}
  Stream.Write (Data^, Length);
  CRC := Byteswap (update_crc(CRC, Data, Length) xor $ffffffff);
  {Writes final CRC}
  Stream.Write(CRC, 4);
end; *)

{Compress and writes IDAT chunk data}
(*
procedure TPng.IDATZlibWrite (var ZLIBStream: TZStreamRec2;
  Buffer: Pointer; const Length: Cardinal);
begin
  with ZLIBStream, ZLIBStream.ZLIB do
    begin
      {Set data to be compressed}
      next_in := Buffer;
      avail_in := Length;
      {Compress all the data avaliable to compress}
      while avail_in > 0 do
        begin
          deflate (ZLIB, Z_NO_FLUSH);
          {The whole buffer was used, save data to stream and restore buffer}
          if avail_out = 0 then
            begin
              {Writes this IDAT chunk}
              WriteIDAT (fStream, Data, MaxIdatSize);
              {Restore buffer}
              next_out := Data;
              avail_out := MaxIdatSize;
            end; {if avail_out = 0}
        end; {while avail_in}
    end; {with ZLIBStream, ZLIBStream.ZLIB}
end;

{Finishes compressing data to write IDAT chunk}
procedure TPng.FinishIDATZlib (var ZLIBStream: TZStreamRec2);
begin
  with ZLIBStream, ZLIBStream.ZLIB do
    begin
      {Set data to be compressed}
      next_in := nil;
      avail_in := 0;
      while deflate (ZLIB,Z_FINISH) <> Z_STREAM_END do
        begin
          {Writes this IDAT chunk}
          WriteIDAT (fStream, Data, MaxIdatSize - avail_out);
          {Re-update buffer}
          next_out := Data;
          avail_out := MaxIdatSize;
        end;
      if avail_out < MaxIdatSize then
        {Writes final IDAT}
        WriteIDAT (fStream, Data, MaxIdatSize - avail_out);
    end; {with ZLIBStream, ZLIBStream.ZLIB}
end;
               *)

function Power (Base, Exponent: Extended): Extended;
begin
  if Exponent = 0.0 then
    Result := 1.0               {Math rule}
  else if (Base = 0) or (Exponent = 0) then
    Result := 0
  else
    Result := Exp (Exponent * Ln (Base));
end;


{Assigns from another object}

procedure TPng.Assign (Source: TObject);
begin
  ClearRenderedFrames;
  if Source = nil then exit
  else if Source is TPng then
    AssignPNG (Source as TPng)
  else
    inherited;
end;

procedure TPng.ClearRenderedFrames;
var
  i : integer;
begin
  for i := 0 to RenderedFrames.Count - 1 do
    TRenderedFrame (RenderedFrames[i]).Free;
  RenderedFrames.Clear;
  FAnimated := false;
  FNosRepeats := 0;
  FNosFrames := 0;
end;

procedure TPng.ClearRenderStreams;
var
  i : integer;
  aFrame : TRenderedFrame;

  function NoLongerNeeded (anIndex : integer) : boolean;
  var
    bFrame : TRenderedFrame;
  begin
    if anIndex >= RenderedFrames.Count - 1 then
      Result := true
    else
      begin
        bFrame := TRenderedFrame (RenderedFrames[anIndex + 1]);
        Result := bFrame.Rendered or (bFrame.Blend = APNG_BLEND_OP_SOURCE) or
                (bFrame.Disposal = APNG_DISPOSE_OP_BACKGROUND);
      end;
  end;

begin
 if RenderedFrames.Count = 1 then
    begin
      aFrame := TRenderedFrame (RenderedFrames[0]);
      if aFrame.Rendered then aFrame.Raw.Clear;
    end
  else
    for i := 0 to RenderedFrames.Count - 1 do
      begin
        aFrame := TRenderedFrame (RenderedFrames[i]);
        if not aFrame.Rendered then continue;
        if NoLongerNeeded (i) then aFrame.Raw.Clear;
      end;
end;

constructor TPng.Create;
begin
  {Let it be created}
  inherited Create;
//  ScratchDC := 0;
//  ScratchHandle := 0;
//  FFormat := aFormat;
  //ScratchData := nil;
  //ScratchAlpha := nil;
  //ScratchWidth := 0;
  //ScratchHeight := 0;

  RenderData := nil;
  RenderAlpha := nil;
  RenderBPR := 0;
  RenderWidth := 0;
  RenderHeight := 0;


  {Initial properties}
 // TempPalette := 0;
  FFilters := [pfSub];
  FCompressionLevel := 7;
  FInterlaceMethod := imNone;
  FMaxIdatSize := High(Word);
  RenderedFrames := TList.Create;
  CurrRenderedFrame := nil;
  LoadFrame := nil;
  FAnimated := false;
end;

{Portable Network Graphics object being destroyed}
destructor TPng.Destroy;
begin
  {Free the temporary palette}

 // delete palette
  ClearRenderedFrames;
  RenderedFrames.Free;

  if RenderData <> nil then FreeMem (RenderData);
  if RenderAlpha <> nil then FreeMem (RenderAlpha);
  inherited Destroy;
end;

procedure TPng.GetPixelInfo (aWidth : integer; var LineSize, Offset: Cardinal);
begin
  LineSize := BytesForPixels (aWidth, SourceIHDR.ColorType, SourceIHDR.BitDepth);
  case SourceIHDR.ColorType of
    COLOR_GRAYSCALE      : if SourceIHDR.BitDepth = 16 then Offset := 2 else Offset := 1;
    COLOR_PALETTE        : offset := 1;
    COLOR_RGB            : offset := 3 * SourceIHDR.BitDepth Div 8;
    COLOR_GRAYSCALEALPHA : offset := 2 * SourceIHDR.BitDepth Div 8;
    COLOR_RGBALPHA       : offset := 4 * SourceIHDR.BitDepth Div 8;
    else Offset := 0;
    end;
end;

{Returns image height}
function TPng.GetHeight: integer;
begin
  Result := SourceIHDR.Height;
end;

{Returns image width}
function TPng.GetWidth: integer;
begin
  Result := SourceIHDR.Width;
end;

function TPng.IsEmpty: Boolean;
begin
  Result := (RenderedFrames.Count = 0);
end;

procedure TPng.DrawPartialTrans (Canvas: TCanvas; Rect : Ultibo.TRect;
  aFrame: TRenderedFrame; anAlpha: byte);

  {Adjust the rectangle structure}
  procedure AdjustRect (var Rect: Ultibo.TRect);
  var
    t: integer;
  begin
    if Rect.Right < Rect.Left then
      begin
        t := Rect.Right;
        Rect.Right := Rect.Left;
        Rect.Left := t;
      end;
    if Rect.Bottom < Rect.Top then
      begin
        t := Rect.Bottom;
        Rect.Bottom := Rect.Top;
        Rect.Top := t;
      end;
  end;

var
  {Buffer bitmap modification}
  BPRSource, BPRDest, BPRAlpha : integer;
  FImageSource : PTripleArray;
  FAlphaSource : PByteArray;
  FImageDest32 : PQuadArray;
  FAlpha : byte;
  i, j, i2 : integer;
  {For bitmap stretching}
  W, H : integer;
  Stretch : Boolean;
  FactorX {, FactorY} : Double;
begin
  if (Canvas = nil) or (aFrame = nil) then exit;
  {Prepares the rectangle structure to stretch draw}
  if (Rect.Right = Rect.Left) or (Rect.Bottom = Rect.Top) then exit;
  AdjustRect (Rect);
  {Gets the width and height}
  W := Rect.Right - Rect.Left;
  H := Rect.Bottom - Rect.Top;
  Stretch := (W <> Width) or (H <> Height);
  if Stretch then FactorX := W / Width else FactorX := 1;
//  if Stretch then FactorY := H / Height else FactorY := 1;
  BPRSource := 3 * Width;        // png is rendered as 24 bit with separate alpha channel
  BPRAlpha := Width;
  {Obtains image pointers}
  FImageSource := aFrame.ImageData;        // these are source pointers
  FAlphaSource := aFrame.ImageAlpha;
  case Canvas.ColourFormat of  // the rest we will deal with when they appear
    COLOR_FORMAT_ARGB32 :  {32 bits per pixel Red/Green/Blue/Alpha (RGBA8888)}
      begin
        BPRDest := 4 * Canvas.Width;  // 32 bit colour format
        FImageDest32 := PQuadArray (Cardinal (Canvas.Buffer) + ((Rect.top * Canvas.Width) + Rect.left) * 4);
        for j := 1 to h do
          begin
            {Process all the pixels in this line}
            for i := 0 to w - 1 do
              begin
                if Stretch then i2 := Trunc (i / FactorX) else i2 := i;
                {Optmize when we don´t have transparency}
                if aFrame.ImageAlpha = nil then
                  begin
                    FImageDest32^[i].a := FImageSource^[i2].a;      // blue
                    FImageDest32^[i].b := FImageSource^[i2].b;      // green
                    FImageDest32^[i].c := FImageSource^[i2].c;
                    FImageDest32^[i].d := 255;                      // alpha
                  end
                else
                  begin
                    FAlpha := (FAlphaSource^[i2] * anAlpha) shr 8;
                   // FAlpha := 120;
                    if (FAlpha <> 0) then // if no alpha then don't draw anything
                      if (FAlpha = 255) then   // if fully opaque
                        begin
                          FImageDest32^[i].a := FImageSource^[i2].a;
                          FImageDest32^[i].b := FImageSource^[i2].b;
                          FImageDest32^[i].c := FImageSource^[i2].c;
                          FImageDest32^[i].d := 255;    // alpha is not used             here ish
                        end
                      else
                        with FImageDest32^[i] do
                          begin
                            a := (255 + FImageSource^[i2].a * FAlpha + a * (not FAlpha)) shr 8;
                            b := (255 + FImageSource^[i2].b * FAlpha + b * (not FAlpha)) shr 8;
                            c := (255 + FImageSource^[i2].c * FAlpha + c * (not FAlpha)) shr 8;
                          end;
                  end;
              end;  // i
            {Move pointers}
            Cardinal (FImageDest32) := Cardinal (FImageDest32) + BPRDest;
    //        if Stretch then j2 := trunc (j / FactorY) else j2 := j;
            Cardinal (FImageSource) := Cardinal (FImageSource) + (BPRSource);
            Cardinal (FAlphaSource) := Cardinal (FAlphaSource) + (BPRAlpha); //  , BPRAlpha * j2);
          end;    // j
      end;  // case
    end;
end;

function TPng.GetMarker (aFrameNos: integer): string;
var
  i : integer;
  aFrame : TRenderedFrame;
begin
  Result := '';
  aFrame := GetRenderedFrame (aFrameNos);
  if aFrame = nil then exit;
  for i := 0 to aFrame.Texts.Count - 1 do
    begin
      if Copy (aFrame.Texts[i], 1, 7) = 'Marker'#0 then
        begin
          Result := Copy (aFrame.Texts[i], 8, length (aFrame.Texts[i]) - 7);
          exit;
        end;
    end;
end;

{Set the maximum size for IDAT chunk}
procedure TPng.SetMaxIdatSize (const Value: integer);
begin
  {Make sure the size is at least 65535}
  if Value < High(Word) then
    fMaxIdatSize := High(Word) else fMaxIdatSize := Value;
end;

{Creates a file stream reading from the filename in the parameter and load}
function TPng.LoadFromFile (const Filename: String) : boolean;
var
  FileStream: TFileStream;
//  aBitmap : Graphics.TBitmap;
//  aJPEG : TJPEGImage;
  Ext : string;
begin
  Log ('PNG Loading ' + Filename + ' from file.');
  Result := false;
  if not FileExists (Filename) then
    begin
      Log ('PNG Load ' + filename + ' does not exist.');
      exit;
    end;
  Ext := UpperCase (ExtractFileExt (FileName));
  if Ext = '.PNG' then
    try
      FileStream := TFileStream.Create (Filename, fmOpenRead);
      Result := LoadFromStream (FileStream);  {Loads the data}
      FileStream.Free;             {Free file stream}
    except
    end
end;

procedure TPng.Draw (aCanvas: TCanvas; const Rect: Ultibo.TRect; aFrameNos : integer);
begin
  Draw (aCanvas, Rect, aFrameNos, 255);
end;

procedure TPng.Draw (aCanvas: TCanvas; const Rect: Ultibo.TRect;
  aFrameNos: integer; anAlpha: byte);

var
  aFrame : TRenderedFrame;
begin
  if IsEmpty then exit;
  aFrame := RenderFrame (aFrameNos);
  if aFrame = nil then exit;
  DrawPartialTrans (aCanvas, Rect, aFrame, anAlpha);
end;

{Draws the image into a canvas}                      // Header.ImageData holds the bitmap
procedure TPng.Draw (aCanvas: TCanvas; const Rect: Ultibo.TRect);
begin
  Draw (aCanvas, Rect, 0, $ff);
end;

{Characters for the header}
const
  PngHeader: Array [0..7] of Char = (#137, #80, #78, #71, #13, #10, #26, #10);

{Loads the image from a stream of data}
function TPng.LoadFromStream (Stream: TStream) : boolean;
var
  Header    : Array[0..7] of Char;
  s : string;
  {Chunks reading}
  ChunkCRC {, CheckCRC} : cardinal;
  ChunkLength: Cardinal;
  ChunkData : Pointer;
  ChunkName  : TChunkName;
  FrameCount : integer;
  PalColor : PPalEntry;
  i : integer;
  aByte : pByte;

 function RenderedFrame (aFrame : integer) : TRenderedFrame;
  begin
    Result := GetRenderedFrame (aFrame);
    if Result = nil then
      begin
        Result := TRenderedFrame.Create (Self);
        Result.FrameNos := aFrame;
        RenderedFrames.Add (Result);
      end;
  end;

begin
  Result := false;
  CurrRenderedFrame := nil;
  LoadFrame := nil;
  ChunkData := nil;
  FrameCount := 0;
  ClearRenderedFrames;
  InitializeGamma;
  Stream.Read (Header[0], 8);
  if Header <> PngHeader then
    begin
      Log ('PNG Invalid Header');
      exit;
    end;
  repeat
    if Stream.Read (ChunkLength, 4) = 0 then
      begin
        Log ('PNG Unexpected End.');
        exit;
      end;

    ChunkLength := ByteSwap (ChunkLength);
    Stream.Read (Chunkname, 4);
    if ChunkLength > 0 then
      begin
        ReAllocMem (ChunkData, ChunkLength);
        Stream.Read (ChunkData^, ChunkLength);
      end;
    Stream.Read (ChunkCRC, 4);
    ChunkCRC := ByteSwap (ChunkCRC);
    if ChunkName = 'IHDR' then
      begin
        if (ChunkLength < SIZEOF (TIHDRData)) then
          begin
            Log ('PNG Invalid Header.');
            exit;
          end;
        SourceIHDR := pIHDRData (ChunkData)^;
        SourceIHDR.Width := ByteSwap (SourceIHDR.Width);
        SourceIHDR.Height := ByteSwap (SourceIHDR.Height);
       if (SourceIHDR.Width > High (Word)) or (SourceIHDR.Height > High (Word)) then
          begin
            Log ('PNG Excessive Size.');
            exit;
          end; {if IHDRData.Width > High(Word)}
        if (SourceIHDR.CompressionMethod <> 0) then
          begin
            Log ('PNG Unknown Compression Method.');
            exit;
          end;
        if (SourceIHDR.InterlaceMethod <> 0) then // and (SourceIHDR.InterlaceMethod <> 1) then
          begin
            Log ('PNG Unknown Interlace Method.');
            exit;
          end;
        fInterlaceMethod := TInterlaceMethod (SourceIHDR.InterlaceMethod);
        fFrameCounter := 0; // animation - reset frame counter
        LoadFrame := RenderedFrame (0);
      end
    else if ChunkName = 'IDAT' then
      begin
        if LoadFrame <> nil then
          begin
            LoadFrame.Raw.Write (ChunkData^, ChunkLength); // no chunk or crc
          end;
      end
    else if ChunkName = 'acTL' then
      begin
        FrameCount := 0;
        FAnimated := true;
        FNosFrames := GetInt (ChunkData, 0);
        FNosRepeats := GetInt (ChunkData, 4);
//        Log ('PNG Frames ' + FNosFrames.ToString + ' Repeats ' + FNosRepeats.ToString);
      end
    else if ChunkName = 'tEXt' then
      begin
        if LoadFrame <> nil then
          begin
            SetLength (s, ChunkLength);
            CopyMemory (@s[1], ChunkData, ChunkLength);
//            Log ('PNG Embedded Text ' + s);
            LoadFrame.Texts.Add (s);
          end;
      end
    else if ChunkName = 'PLTE' then
      begin
//        Log ('PNG Has Palette.');
        if (ChunkLength mod 3 <> 0) or (ChunkLength div 3 > 256) then
          begin
            Log ('PNG Invalid Palette.');
            exit;
          end; {if Size mod 3 <> 0}
        PalColor := ChunkData;
        PaletteCount := ChunkLength div 3;
//        Log ('PNG Palette Count ' + PaletteCount.ToString);
        SetLength (PaletteValues, PaletteCount);
        for i := 0 to PaletteCount - 1 do
          with PaletteValues[i] do
            begin
              a := GammaTable[PalColor^.r];
              b := GammaTable[PalColor^.g];
              c := GammaTable[PalColor^.b];
              d := 255;
              inc (PalColor);
//              Log (format ('PNG Palette %d (%d,%d,%d)', [i, a, b, c]));
            end;
      end
    else if ChunkName = 'tRNS' then
      begin
//        Log ('PNG Transparentcy size = ' + inttostr (ChunkLength));
        if (ChunkLength = PaletteCount) then
          begin
            aByte := ChunkData;
            for i := 0 to PaletteCount - 1 do
              begin
                PaletteValues[i].d := aByte^;
                inc (aByte);
              end;
          end;
      end
    else if ChunkName = 'fcTL' then
      begin
        FrameCount := FrameCount + 1;
 //       Log ('Render Frame ' + FrameCount.ToString);
        LoadFrame := RenderedFrame (FrameCount);
        LoadFrame.Width := GetInt (ChunkData, 4);
        LoadFrame.Height := GetInt (ChunkData, 8);
        LoadFrame.X := GetInt (ChunkData, 12);
        LoadFrame.Y := GetInt (ChunkData, 16);
        LoadFrame.Delay := GetInt (ChunkData, 20);        // split into two
        LoadFrame.Disposal := GetByte (ChunkData, 24);
        LoadFrame.Blend := GetByte (ChunkData, 25);
      end
    else if ChunkName = 'fdAT' then
      begin
        if LoadFrame <> nil then
          LoadFrame.Raw.Write (GetData (ChunkData, 4)^, ChunkLength - 4); // no chunk or crc
      end;
  until ChunkName = 'IEND';
  CurrRenderedFrame := RenderFrame (0);    // render default image
//  if AlphaScanLine[0] = nil then CreateAlpha (CurrRenderedFrame);
end;

function TPng.NextFrame (From: integer; var Dir : integer): integer;
var
  aStr : string;
begin
  aStr := GetMarker (From);
  if aStr = 'Up' then Dir := dirUp
  else if aStr = 'Down' then Dir := dirDown
  else if aStr = 'Stop' then Dir := dirStop;
  case Dir of
    dirUp    : Result := From + 1;
    dirDown  : Result := From - 1;
    dirUp2   : Result := From + 2;
    dirDown2 : Result := From - 2;
    dirStop  : Result := From;
    else Result := From;
  end;
  if Result > NosFrames then Result := Result - NosFrames; // circle around
  if Result < 1 then Result := NosFrames + Result;
end;

function TPng.NosFrames: integer;
begin
  Result := fNosFrames;
end;

function TPng.NosRepeats: integer;
begin
  Result := fNosRepeats;
end;

{Changing height is not supported}
procedure TPng.SetHeight (Value: integer);
begin
  Log ('Change of height is not supported.');
end;

{Changing width is not supported}
procedure TPng.SetWidth (Value: integer);
begin
  Log ('Change of width is not supported.');
end;

{Assigns this TPng to another object}
procedure TPng.AssignTo (Dest: TObject);
begin
  if Dest is TPng then TPng(Dest).AssignPNG (Self)
 end;

{Assigns from another PNG}
procedure TPng.AssignPNG (Source: TPng);
begin
  // TODO
end;

function TPng.GetDelay (aFrameNos : integer): integer;
var
  aFrame : TRenderedFrame;
begin
  Result := 0;
  aFrame := GetRenderedFrame (aFrameNos);
  if aFrame = nil then exit;
  Result := aFrame.Delay;
end;

function TPng.IsAnimated: boolean;
begin
  Result := FAnimated;
end;

{Initialize gamma table}
procedure TPng.InitializeGamma;
var
  i: integer;
begin
  {Build gamma table as if there was no gamma}
  for i := 0 to 255 do
    begin
      GammaTable[i] := i;
      InverseGamma[i] := i;
    end; {for i}
end;

{Returns the transparency mode used by this png}
function TPng.GetTransparencyMode: TPNGTransparencyMode;
begin
  Result := ptmPartial;
(*  with OutputIHDR do
    begin
      Result := ptmNone; {Default result}
      {Gets the TRNS chunk pointer}
      TRNS := Chunks.ItemFromClass(TChunkTRNS) as TChunkTRNS;

      {Test depending on the color type}
      case ColorType of
        {This modes are always partial}
        COLOR_RGBALPHA, COLOR_GRAYSCALEALPHA: Result := ptmPartial;
        {This modes support bit transparency}
        COLOR_RGB, COLOR_GRAYSCALE: if TRNS <> nil then Result := ptmBit;
        {Supports booth translucid and bit}
        COLOR_PALETTE:
          {A TRNS chunk must be present, otherwise it won't support transparency}
         // if TRNS <> nil then
          //  if TRNS.BitTransparency then
            //  Result := ptmBit else
              Result := ptmPartial;
        end; {case}
    end; {with Header}      *)
end;

procedure TPng.RenderAllFrames;
var
  i : integer;
begin
  for i := 0 to RenderedFrames.Count - 1 do RenderFrame (i);
  for i := 0 to RenderedFrames.Count - 1 do TRenderedFrame (RenderedFrames[i]).Raw.Clear;
end;

function TPng.RenderFrame (aFrameNos: integer): TRenderedFrame;
var
  aFrame : TRenderedFrame;
  i, j : integer;
  ScratchData : pointer;
  ScratchAlpha : pointer;
  ScratchWidth, ScratchHeight : integer;
  ScratchBPR : integer;

begin              // in here should render any palette into 24bit RGB
  Result := GetRenderedFrame (aFrameNos);
  if Result <> nil then
    begin
      if Result.Rendered then exit; // already rendered
    end
  else
    begin
      Result := TRenderedFrame.Create (Self);
      Result.FrameNos := aFrameNos;
      RenderedFrames.Add (Result);
    end;
//  Log ('PNG Rendering frame ' + IntToStr (aFrameNos));
  BytesPerRow := 3 * Width;    // rendered as RGB
  GetMem (Result.ImageAlpha, Width * Height);
  FillChar (Result.ImageAlpha^, Width * Height, 0);
  GetMem (Result.imageData, BytesPerRow * Height); // allocate memory for main bitmap
  ZeroMemory (Result.ImageData, BytesPerRow * Height);
  if aFrameNos = 0 then  // default image (IDATs only )
    begin
      RenderData := Result.ImageData;
      RenderAlpha := Result.ImageAlpha;
      RenderWidth := Width;    //
      RenderHeight := Height;
      RenderBPR := BytesPerRow;
      if Result.Raw.Size > 0 then RenderStream (Result.Raw)       // just image data
      else if GetRenderedFrame (1) <> nil then RenderStream (GetRenderedFrame (1).Raw);
    end
  else
    begin // render a frame
      j := 0;
      for i := 0 to aFrameNos do
        begin
          if i >= RenderedFrames.Count then
            break
          else
            begin        // look for the last fraem that was complete
              aFrame := TRenderedFrame (RenderedFrames[i]);
              if (aFrame.Disposal = APNG_DISPOSE_OP_BACKGROUND) or
                (aFrame.Blend = APNG_BLEND_OP_SOURCE) then j := i;
            end;
        end;
      for i := j to aFrameNos do
        begin
          aFrame := TRenderedFrame (RenderedFrames[i]);
          if aFrame.Raw.Size = 0 then continue;   // already rendered
          if (aFrame.Width <> Width) or (aFrame.Height <> Height) then
            begin                            // this is a smaller sub frame
              ScratchWidth := aFrame.Width;
              ScratchHeight := aFrame.Height;
              ScratchBPR := ScratchWidth * 3;

              GetMem (ScratchData, ScratchWidth * ScratchHeight * 3); // 24 bit colours
              ZeroMemory (ScratchData, ScratchWidth * ScratchHeight * 3);

              GetMem (ScratchAlpha, ScratchWidth * ScratchHeight);
              ZeroMemory (ScratchAlpha, ScratchWidth * ScratchHeight);

              RenderData := ScratchData;
              RenderAlpha := ScratchAlpha;
              RenderWidth := ScratchWidth;
              RenderHeight := ScratchHeight;
              RenderBPR := ScratchBPR;
              RenderStream (aFrame.Raw);
              FreeMem (ScratchData);
              FreeMem (ScratchAlpha);
            end
          else
            begin         // complete frame
              RenderData := Result.ImageData;
              RenderAlpha := Result.ImageAlpha;
              RenderWidth := Width;
              RenderHeight := Height;
              RenderBPR := BytesPerRow;
              RenderStream (aFrame.Raw);
            end;
        end;
    end;
  with Result do
    begin
      Width := Self.Width;
      Height := Self.Height;
      X := 0;
      Y := 0;
      BitDepth := SourceIHDR.BitDepth;
      ColorType := SourceIHDR.ColorType;
      Blend := APNG_BLEND_OP_SOURCE;
      Disposal := APNG_DISPOSE_OP_BACKGROUND;
      Raw.Clear;
//      UnRenderFrame (Raw, Result);  // save rendered frame as source
    end;       
  Result.Rendered := true;
  ClearRenderStreams;
end;

procedure TPng.RenderStream (Stream: TStream);
var
  ZLIBStream: TZStreamRec2;
begin
  if Stream.Size = 0 then exit;
  Stream.Seek (0,soFromBeginning);
  GetPixelInfo (RenderWidth, FRow_Bytes, FOffset); {Obtain line information} //
//  Log ('Render width ' + RenderWidth.ToString + ' FRow Bytes ' + FRow_Bytes.ToString + ' FOffset ' + FOffset.ToString);
  ZLIBStream := ZLIBInitInflate (Stream);
  GetMem (FRow_Buffer[false], FRow_Bytes + 1);
  GetMem (FRow_Buffer[true], FRow_Bytes + 1);
  ZeroMemory (FRow_Buffer[false], FRow_Bytes + 1);
  FRowUsed := true;
  case FInterlaceMethod of
    imNone  : DecodeNonInterlaced (Stream, ZLIBStream);
    imAdam7 : DecodeInterlacedAdam7 (Stream, ZLIBStream);
    end;
  ZLIBTerminateInflate (ZLIBStream);
  FreeMem (FRow_Buffer[false], FRow_Bytes + 1);
  FreeMem (FRow_Buffer[true], FRow_Bytes + 1);
end;

{Generates alpha information}
procedure TPng.CreateAlpha (aFrame : TRenderedFrame);
begin
  if aFrame = nil then exit;
  with SourceIHDR do
    case ColorType of
      {Png allocates different memory space to hold alpha information}
      {for these types}
      COLOR_GRAYSCALE, COLOR_RGB:
        begin
          if ColorType = COLOR_GRAYSCALE then
            ColorType := COLOR_GRAYSCALEALPHA
          else
          GetMem (aFrame.ImageAlpha, integer (Width) * integer (Height));
          FillChar (aFrame.ImageAlpha^, integer (Width) * integer (Height), #255);
        end;

        (*
      {Palette uses the TChunktRNS to store alpha}
      COLOR_PALETTE:
        begin
          {Gets/creates TRNS chunk}
          if Chunks.ItemFromClass(TChunkTRNS) = nil then
            TRNS := Chunks.Add (TChunkTRNS) as TChunkTRNS
          else
            TRNS := Chunks.ItemFromClass (TChunkTRNS) as TChunkTRNS;
            {Prepares the TRNS chunk}
            with TRNS do
              begin
                Fillchar (PaletteValues[0], 256, 255);
                fDataSize := 1 shl IHDRData.BitDepth;
                fBitTransparency := False
              end; {with Chunks.Add}
          end; *)
    end; {case Header.ColorType}
end;

function TPng.HitTest (aFrameNos, x, y: integer): boolean;
var
  aFrame : TRenderedFrame;
  PBytes : PByteArray;
begin
//  Log ('Hit test x ' + inttostr (x) + ' y ' + inttostr (y) + ' Frame ' + inttostr (aFrameNos));
  Result := false;
  if (x < 0) or (x > Width - 1) then exit;
  if (y < 0) or (y > Height - 1) then exit;
  aFrame := GetRenderedFrame (aFrameNos);
  if aFrame = nil then aFrame := RenderFrame (aFrameNos);
  if aFrame = nil then exit;
  with SourceIHDR do
    if (ColorType = COLOR_RGBALPHA) or (ColorType = COLOR_GRAYSCALEALPHA) then
      Cardinal (PBytes) := Cardinal (aFrame.ImageAlpha) + (y * Width)
    else
      begin
        Result := true;
        exit;
      end;
  Result := PBytes^[x] <> 0;
end;

function TPng.GetRenderedFrame (aFrameNos: integer): TRenderedFrame;
var
  i : integer;
begin
  for i := 0 to RenderedFrames.Count - 1 do
    begin
      Result := TRenderedFrame (RenderedFrames[i]);
      if Result.FrameNos = aFrameNos then exit;
    end;
  Result := nil;
end;

{ TRenderedFrame }

constructor TRenderedFrame.Create (anOwner : TPng);
begin
  fOwner := anOwner;
  FrameNos := -1;
  Modified := false;
  Texts := TStringList.Create;
  Raw := TMemoryStream.Create;
  ImageData := nil;
  ImageAlpha := nil;
  Rendered := false;
  Width := anOwner.Width;
  Height := anOwner.Height;
  X := 0;
  Y := 0;
  Delay := 0;
  Disposal := APNG_DISPOSE_OP_NONE;
  Blend := APNG_BLEND_OP_SOURCE;
  RawColorType := COLOR_RGB;
  RawBitDepth := 8;
end;

destructor TRenderedFrame.Destroy;
begin
  Raw.Free;
  Texts.Free;
  if ImageData <> nil then FreeMem (ImageData);
  if ImageAlpha <> nil then FreeMem (ImageAlpha);
  inherited;
end;

function TRenderedFrame.GetAlphaScanline (const LineIndex: integer): PByteArray;
begin
  if not Rendered then
    begin
      Result := nil;
      exit;
    end;
  Cardinal (Result) := Cardinal (ImageAlpha) + (LineIndex * Width);
end;

function TRenderedFrame.GetPixels (const aX, aY: integer): Cardinal;
begin
  Result := 0;
  if not Rendered then exit;
//  if ((aX >= 0) and (aX <= Width - 1)) and ((aY >= 0) and (aY <= Height - 1)) then
//    with pRGBLine (GetScanline(aY))^[aX] do
 //     Result := RGB (rgbtRed, rgbtGreen, rgbtBlue)
end;

function TRenderedFrame.GetScanline (const LineIndex: integer): Pointer;
var
  BPR : integer;
begin

  Result := nil;
  if not Rendered then exit;
  BPR := (((24 * Width) + 31) and not 31) div 8; {Number of bytes for each image row in destination}
  cardinal (Result) := (cardinal (ImageData) + (Height - 1) * BPR) - (LineIndex * BPR);
end;

procedure TRenderedFrame.SetPixels (const aX, aY: integer; const Value: Cardinal);
begin
  if not Rendered then exit;
//  if ((aX >= 0) and (aX <= Width - 1)) and ((aY >= 0) and (aY <= Height - 1)) then
//    with pRGBLine(GetScanline(aY))^[aX] do
 //     begin

 //       rgbtRed := GetRValue(Value);
   //     rgbtGreen := GetGValue(Value);
     //   rgbtBlue := GetBValue(Value)
    //  end;
end;

end.


