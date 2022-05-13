unit uReadBin;

{$mode objfpc}{$H+}

{ Example 08 File Handling                                                     }
{ Used to Read a bin file                                                      } 

{Declare some units used by this example.}
interface

uses
 
  GlobalConst,
  GlobalTypes,
  Platform,
  Classes,
  Threads,
  SysUtils,
	FileSystem,  {Include the file system core and interfaces}
  FATFS,       {Include the FAT file system driver}
  MMC;         {Include the MMC/SD core to access our SD card}

type
	Buffer = array[0..4159] of Char;
	BufPtr = ^Buffer;
	{A window handle plus a couple of others.}
var
 Count:Integer;
 Filename:String;


	SearchRec:TSearchRec;
 StringList:TStringList;
 	FileStream:TFileStream;
 	//FileStreamlog:TFileStream;
 //WindowHandle:TWindowHandle;
	B : Buffer;
 BP : BufPtr;
 PP : Pointer;
 i,j: integer;
 Characters : String;
 PCharacters : ^PString;
 

procedure SetFileName(const aFileName : String);

procedure SetStingList(aStringList : TStringList);

function ReadBuffer(aIndex:Integer):String;

function Readit(aIndex:Integer):String;

implementation

procedure SetFileName(const aFileName:String);
begin
	FileName:=aFileName;
        WriteLn(FileName);
end;

procedure SetStingList(aStringList : TStringList);
begin
StringList:=aStringList;
end;

function Readit(aIndex:Integer):String;


 begin

	try
   	FileStream:=TFileStream.Create(Filename,fmOpenReadWrite);
   	//FileStreamlog:=TFileStream.Create(Filenamelog,fmOpenReadWrite);
   	{Recreate our string list}
   	StringList:=TStringList.Create;


   	{And use LoadFromStream to read it}

   	StringList.LoadFromStream(FileStream);


   	{PP is Pointer BP is a Pointer to an Buffer = array[0..4159] of Char; }
   	PP:=StringList.GetText;
   	BP:=PP;


   	j:=Length(BP[0]);


    	   Characters:=Copy(BP[0],aIndex,65);




   FileStream.Free;
   StringList.Free;

   {If you remove the SD card and put in back in your computer, you should see the
    file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
    see the contents exactly as they appeared on screen.}
  except
   {TFileStream couldn't open the file}
   //ConsoleWindowWriteLn(WindowHandle,'Failed to open the file ' + Filename);
  end;
  Result:=Characters;
  end;
function ReadBuffer(aIndex:Integer):String;


 begin

   j:=Length(BP[0]);
 

   Characters:=Copy(BP[0],aIndex,65);
  
    
  Result:=Characters;
  end;

  end.

