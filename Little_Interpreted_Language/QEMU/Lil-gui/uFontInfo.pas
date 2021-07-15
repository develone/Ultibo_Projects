unit uFontInfo;

{$mode objfpc}
{$H+}
{$inline on}

interface

uses
  Classes, SysUtils;

type

  { TFontInfo }

  TFontInfo = class
    FileName : string;
    Copyright : string;
    FamilyName : string;
    SubFamilyName : string;
    MajorVersion, MinorVersion : Word;
    Stream : TMemoryStream;
    constructor Create;
    destructor Destroy; override;
  end;
  PFontInfo = ^TFontInfo;


function FontInfo (fn : string; var Info : TFontInfo) : boolean;

implementation

uses ulog;

type

(*  0 	Copyright notice.
    1 	Font Family name.
    2 	Font Subfamily name. Font style (italic, oblique) and weight (light, bold, black, etc.). A font with no particular differences in weight or style (e.g. medium weight, not italic) should have the string "Regular" stored in this position.
    3 	Unique font identifier. Usually similar to 4 but with enough additional information to be globally unique. Often includes information from Id 8 and Id 0.
    4 	Full font name. This should be a combination of strings 1 and 2. Exception: if the font is “Regular” as indicated in string 2, then use only the family name contained in string 1. This is the font name that Windows will expose to users.
    5 	Version string. Must begin with the syntax ‘Version n.nn ‘ (upper case, lower case, or mixed, with a space following the number).
    6 	Postscript name for the font.  *)

  TT_OFFSET_TABLE = record
    uMajorVersion,
    uMinorVersion,
    uNumOfTables,
    uSearchRange,
    uEntrySelector,
    uRangeShift : Word;
  end;

  // Tables in TTF file and theit placement and name (tag)
  TT_TABLE_DIRECTORY = record
    szTag : array [0..3] of Char;         // table name
    uCheckSum,                            // Check sum
    uOffset,                              // Offset from beginning of file
    uLength : Cardinal;                   // length of the table in bytes
  end;

  // Header of names table
  TT_NAME_TABLE_HEADER = record
    uFSelector,                           // format selector. Always 0
    uNRCount,                             // Name Records count
    uStorageOffset : Word;                // Offset for strings storage,
  end;                                    // from start of the table

  // Record in names table
  TT_NAME_RECORD = record
    uPlatformID,
    uEncodingID,
    uLanguageID,
    uNameID,
    uStringLength,
    uStringOffset : Word;                 // from start of storage area
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

function FontInfo (fn :  string; var Info : TFontInfo) : boolean;
var
  f : TFileStream;
  ot : TT_OFFSET_TABLE;
  tb : TT_TABLE_DIRECTORY;
  nth : TT_NAME_TABLE_HEADER;
  nr : TT_NAME_RECORD;
  i, j : integer;
  p : int64;
  a : string;
begin
  Result := false;
  Info.Copyright := '';
  Info.FamilyName := '';
  Info.FileName := '';
  Info.SubFamilyName := '';
  Info.MajorVersion := 0;
  Info.MinorVersion := 0;
  ot.uNumOfTables := 0;    // prevent not initialised warning
  tb.uCheckSum := 0;       // prevent not initialised warning
  nth.uNRCount := 0;       // prevent not initialised warning
  nr.uNameID := 0;         // prevent not initialised warning
  if ExtractFileExt (fn) = '' then fn := fn + '.ttf';
  if not FileExists (fn) then exit;
  Info.FileName := fn;
  try
    f := TFileStream.Create (fn, fmOpenRead);
    try
      f.Seek (0, soFromBeginning);
      f.Read (ot, SizeOf (TT_OFFSET_TABLE));
      ot.uNumOfTables := ByteSwap16 (ot.uNumOfTables);
      Info.MajorVersion := ByteSwap16 (ot.uMajorVersion);
      Info.MinorVersion := ByteSwap16 (ot.uMinorVersion);
      for i := 1 to ot.uNumOfTables do
        begin
          f.Read (tb, SizeOf (TT_TABLE_DIRECTORY));
          if CompareText (string (tb.szTag), 'name')= 0 then
            begin
              tb.uLength := ByteSwap (tb.uLength);
              tb.uOffset := ByteSwap (tb.uOffset);
              f.Seek (tb.uOffset, soFromBeginning);
              f.Read (nth, SizeOf (TT_NAME_TABLE_HEADER));
              nth.uNRCount := ByteSwap16 (nth.uNRCount);
              nth.uStorageOffset := ByteSwap16 (nth.uStorageOffset);
              for j := 1 to nth.uNRCount do
                begin
                  f.Read (nr, SizeOf (TT_NAME_RECORD));
                  nr.uNameID := ByteSwap16 (nr.uNameID);
                  nr.uStringLength := ByteSwap16 (nr.uStringLength);
                  nr.uStringOffset := ByteSwap16 (nr.uStringOffset);
                  nr.uEncodingID := ByteSwap16 (nr.uEncodingID);
                  nr.uLanguageID := ByteSwap16 (nr.uLanguageID);
                  p := f.Position;
                  f.Seek (tb.uOffset + nth.uStorageOffset + nr.uStringOffset, soFromBeginning);
                  SetLength (a, nr.uStringLength);
                  f.Read (a[1], nr.uStringLength);
                  if nr.uEncodingID = 0 then
                    case nr.uNameID of
                      0 : Info.Copyright := a;
                      1 : Info.FamilyName := a;
                      2 : Info.SubFamilyName := a;
                      end;
                  f.Seek (p, soFromBeginning);
                end;
              Result := true;
              break;
            end;
        end;
    finally
      f.Free;
      end;
  except
    end;
end;

{ TFontInfo }

constructor TFontInfo.Create;
begin
  Stream := nil;
end;

destructor TFontInfo.Destroy;
begin
  if Assigned (Stream) then Stream.Free;
  inherited Destroy;
end;

end.

