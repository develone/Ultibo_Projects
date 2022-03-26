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

 

procedure SetFileName(const aFileName : String);

procedure SetStingList(aStringList : TStringList);


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
   	//StringListlog:=TStringList.Create;

   	{And use LoadFromStream to read it}
   	//ConsoleWindowWriteLn(WindowHandle,'Loading the TStringList from the file');
   	StringList.LoadFromStream(FileStream);
   	//ConsoleWindowWriteLn(WindowHandle,'Num of strings in file StringList.Count '+intToStr(StringList.Count));

   	{PP is Pointer BP is a Pointer to an Buffer = array[0..40159] of Char; }
   	PP:=StringList.GetText;
   	BP:=PP;


   	j:=Length(BP[0]);
   	//ConsoleWindowWriteLn(WindowHandle,'Length '+intToStr(j));
   	//ConsoleWindowWriteLn(WindowHandle,'This is all chars '+BP[0]);
   	//StringListlog.add(BP[0]);

    	   Characters:=Copy(BP[0],aIndex,65);
           //WriteLn(intToStr(aIndex)+' '+Characters);
    	   //CRC:=Copy(Characters,65,1);
    	   //ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+Characters+' '+CRC);
    	   //ConsoleWindowWriteLn(WindowHandle,intToStr(i)+' '+Characters);
    	   //StringList.add(intToStr(i)+' '+Characters);


    //ConsoleWindowWriteLn(WindowHandle,'Closing the file');
   //ConsoleWindowWriteLn(WindowHandle,'');
   FileStream.Free;
   StringList.Free;
   //FileStreamlog.Free;
   //StringListlog.Free;
   {If you remove the SD card and put in back in your computer, you should see the
    file "Example 08 File Handling.txt" on it. If you open it in a notepad you should
    see the contents exactly as they appeared on screen.}
  except
   {TFileStream couldn't open the file}
   //ConsoleWindowWriteLn(WindowHandle,'Failed to open the file ' + Filename);
  end;
  Result:=Characters;
  end;

  end.

