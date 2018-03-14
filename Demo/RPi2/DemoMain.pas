unit DemoMain;

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}

interface

{==============================================================================}
uses GlobalConfig,
     GlobalConst,
     GlobalTypes,
     Platform,
     Threads,
     SysUtils,
     Classes,
     Ultibo,
     UltiboClasses,
     UltiboUtils,
     Winsock2,
     HeapManager,
     Devices,
     USB,
     MMC,
     Network,
     Storage,
     FileSystem,
     Keyboard,
     Mouse,
     Console,
     HTTP,
     Framebuffer,
     Font,
     Logging,
     Timezone,
     Services,
     Shell,           
     ShellFilesystem, 
     ShellUpdate,
     RemoteShell,
     uTFTP,
     WebStatus;

{==============================================================================}
const
 DEMO_READY_DELAY = 3000;
 DEMO_STAGE_DELAY = 500;
 DEMO_THREAD_DELAY = 150;
 DEMO_SCROLL_DELAY = 50;
 DEMO_REFRESH_DELAY = 250;
 DEMO_KEYPRESS_DELAY = 100;
 
 UTILIZATION_THREAD_DELAY = 250;
 
 MESSAGE_DELIMITER = Chr(32);         {Space}
 MESSAGE_LINEEND = Chr(13) + Chr(10); {CR LF}
 MESSAGE_DOUBLEEND = MESSAGE_LINEEND + MESSAGE_DELIMITER + MESSAGE_LINEEND;
 
 MESSAGE_ID_NONE                      = 0;
 MESSAGE_ID_BANNER                    = 1;
 MESSAGE_ID_WELCOME                   = 2;
 MESSAGE_ID_INTRO                     = 3;
 MESSAGE_ID_INTRO_DONE                = 4;
 MESSAGE_ID_BLINKER                   = 5;
 MESSAGE_ID_BLINKER_DONE              = 6;
 MESSAGE_ID_CONSOLE                   = 7;
 MESSAGE_ID_CONSOLE_TOP_RIGHT         = 8;
 MESSAGE_ID_CONSOLE_BOTTOM_LEFT       = 9;
 MESSAGE_ID_CONSOLE_BOTTOM_RIGHT      = 10;
 MESSAGE_ID_CONSOLE_BOTTOM            = 11;
 MESSAGE_ID_CONSOLE_RIGHT             = 12;
 MESSAGE_ID_CONSOLE_SCROLLUP          = 13;
 MESSAGE_ID_CONSOLE_SCROLLDOWN        = 14;
 MESSAGE_ID_CONSOLE_DONE              = 15;
 MESSAGE_ID_THREADS                   = 16;
 MESSAGE_ID_THREADS_COUNT             = 17;
 MESSAGE_ID_THREADS_CREATE            = 18;
 MESSAGE_ID_THREADS_RUN               = 19;
 MESSAGE_ID_THREADS_DESTROY           = 88;
 MESSAGE_ID_THREADS_DONE              = 20;
 MESSAGE_ID_DATETIME                  = 21;
 MESSAGE_ID_DATETIME_FORMAT           = 22;
 MESSAGE_ID_DATETIME_TIMEZONE         = 23;
 MESSAGE_ID_DATETIME_DONE             = 24;
 MESSAGE_ID_FILES                     = 25;
 MESSAGE_ID_FILES_DIR                 = 26;
 MESSAGE_ID_FILES_CREATE              = 27;
 MESSAGE_ID_FILES_TYPE                = 28;
 MESSAGE_ID_FILES_DONE                = 29;

 MESSAGE_ID_COMPLETED                 = 99;
 MESSAGE_ID_COMPLETED_DONE            = 100;
 MESSAGE_ID_ULTIBO                    = 101;
 MESSAGE_ID_ULTIBO_ORG                = 102;
 
{==============================================================================}
type
 PDemoBlock = ^TDemoBlock;
 TDemoBlock = record
  Value:Pointer;
  Next:PDemoBlock;
 end;
 
 TDemoThread = class(TThread)
  constructor Create(WindowHandle:TWindowHandle);
 private
  FWindowHandle:TWindowHandle;
 public
  procedure Execute; override;
 end; 
 
 TUtilizationThread = class(TThread)
  constructor Create(WindowHandle:TWindowHandle);
 private
  FWindowHandle:TWindowHandle;
 public
  procedure Execute; override;
 end; 
 
{==============================================================================}
var
 HTTPListener:THTTPListener;

{==============================================================================}

function InitDemo:Boolean;
function RunDemo:Boolean;
     
function ShowWelcome:Boolean;
function ShowIntroduction:Boolean;
function ShowBlinker:Boolean;
function ShowConsole:Boolean;
function ShowThreads:Boolean;
function ShowDateTime:Boolean;
function ShowFiles:Boolean;
function ShowCompleted(var Rerun:Boolean):Boolean;

{==============================================================================}

function GetMessageText(Id:LongWord):String;
function GetMessageDelay(Id:LongWord):LongWord;

function GetBoardDescription:String;
function GetBoardTotalMemory:String;
function GetBoardAvailableMemory:String;
function GetBoardUsedMemory:String;
function GetBoardCPUs:String;
function GetBoardTemperature:String;
function GetBoardCPUClock:String;

function GetNetworkAddress:String;
function GetNetworkConnected:Boolean;

function GetKeyboardKeypressed(Timeout:LongWord):Boolean;

function ConsoleWriteMessage(Handle:TWindowHandle;const Text:String):Boolean;
function ConsoleWriteSlowMotion(Handle:TWindowHandle;X,Y:LongWord;const Text:String;Delay:LongWord):Boolean;

{==============================================================================}
{==============================================================================}
     
implementation

{==============================================================================}
{==============================================================================}

constructor TDemoThread.Create(WindowHandle:TWindowHandle);
begin
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE); {Create suspended}
 FWindowHandle:=WindowHandle;
end;

{==============================================================================}

procedure TDemoThread.Execute; 
var
 Prev:PDemoBlock;
 Next:PDemoBlock;
 Block:PDemoBlock;
begin
 {}
 Block:=nil;
 Prev:=nil;
 
 while not Terminated do
  begin
   if CPUGetCount > 1 then
    begin
     ConsoleWindowWriteLn(FWindowHandle,'Thread ' + IntToHex(ThreadGetCurrent,8) + ' on CPU' + IntToStr(CPUGetCurrent) + ' writing to the console at ' + TimeToStr(Time));
    end
   else
    begin
     ConsoleWindowWriteLn(FWindowHandle,'Thread ' + IntToHex(ThreadGetCurrent,8) + ' writing to the console at ' + TimeToStr(Time));
    end;
   
   Next:=GetMem(SizeOf(TDemoBlock) + LongWord((GetTickCount64 mod 50) * SIZE_2K));
   if Next <> nil then
    begin
     Next.Value:=Pointer(LongWord(Next) + SizeOf(TDemoBlock));
     Next.Next:=nil;
     
     if Block = nil then
      begin
       Block:=Next;
       Prev:=Block;
      end
     else
      begin
       Prev.Next:=Next;
       Prev:=Next;
      end;      
    end;  
   
   Sleep(DEMO_THREAD_DELAY);
  end;
  
 if Block <> nil then
  begin
   Next:=Block;
   while Next <> nil do
    begin
     Prev:=Next;
     Next:=Prev.Next;
     
     FreeMem(Prev);
    end;
  end;
  
 ConsoleWindowWriteLn(FWindowHandle,'Thread ' + IntToHex(ThreadGetCurrent,8) + ' terminating at ' + TimeToStr(Time));
 
 Sleep(DEMO_THREAD_DELAY);
end;

{==============================================================================}
{==============================================================================}

constructor TUtilizationThread.Create(WindowHandle:TWindowHandle);
begin
 inherited Create(True,THREAD_STACK_DEFAULT_SIZE); {Create suspended}
 FWindowHandle:=WindowHandle;
 FreeOnTerminate:=True;
 Start;
end;

{==============================================================================}

procedure TUtilizationThread.Execute; 
var
 Y:LongWord;
 Rows:LongWord;
 Count:LongWord;
begin
 {}
 {Get current rows}
 Rows:=ConsoleWindowGetRows(FWindowHandle);
 
 {Center the output}
 Y:=(Rows - CPUGetCount) div 2;
 
 while not Terminated do
  begin
   {Set the position}
   ConsoleWindowSetX(FWindowHandle,1);
   ConsoleWindowSetY(FWindowHandle,Y);
   
   {Get the utilization}
   for Count:=0 to CPUGetCount - 1 do
    begin
     ConsoleWindowWriteLn(FWindowHandle,' CPU' + IntToStr(Count) + ' utilization is ' + FloatToStr(CPUGetPercentage(Count)) + '%   '); 
    end;
   
   Sleep(UTILIZATION_THREAD_DELAY);
  end;
  
 ConsoleWindowDestroy(FWindowHandle); 
end;

{==============================================================================}
{==============================================================================}
     
function InitDemo:Boolean;
begin
 {}
 Result:=False;
 
 {Create and start HTTP Listener}
 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 
 {Register Web Status}
 WebStatusRegister(HTTPListener,'','',True);
 
 Result:=True;
end;

{==============================================================================}

function RunDemo:Boolean;
var
 Rerun:Boolean;
begin
 {}
 Result:=False;
 
 Rerun:=False;
 repeat
  {Wait about 3 seconds for the HDMI to be ready}
  Sleep(DEMO_READY_DELAY);
  
  {Show welcome}
  ShowWelcome;
  
  Sleep(DEMO_STAGE_DELAY);
  
  {Show intro}
  ShowIntroduction;
  
  Sleep(DEMO_STAGE_DELAY);
 
  {Show blinker}
  ShowBlinker;
 
  Sleep(DEMO_STAGE_DELAY);
 
  {Show console}
  ShowConsole;
  
  Sleep(DEMO_STAGE_DELAY);
 
  {Show threads}
  ShowThreads;
  
  Sleep(DEMO_STAGE_DELAY);
 
  {Check Clock}
  if ClockGetTime > TIME_TICKS_TO_1970 then
   begin
    {Show datetime} 
    ShowDateTime;
    
    Sleep(DEMO_STAGE_DELAY);
   end; 
  
  {Show files}
  ShowFiles;
  
  Sleep(DEMO_STAGE_DELAY);
 
  {Show completed}
  ShowCompleted(Rerun);
  
 until Rerun = False;
 
 Result:=True;
end;
     
{==============================================================================}
     
function ShowWelcome:Boolean;
var
 X:LongWord;
 Y:LongWord;
 Rows:LongWord;
 Cols:LongWord;
 Buffer:String;
 Window:TWindowHandle;
 Viewport:TConsoleRect;
begin
 {}
 Result:=False;
 
 {Create a Window}
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_FULL,True);
 
 {Get Rows/Cols}
 Rows:=ConsoleWindowGetRows(Window);
 Cols:=ConsoleWindowGetCols(Window);
 
 {Create a Viewport}
 Viewport.X1:=Cols div 4;
 Viewport.X2:=(Cols div 4) * 3;
 Viewport.Y1:=Rows div 4;
 Viewport.Y2:=(Rows div 4) * 3;
 ConsoleWindowSetRect(Window,Viewport);
 
 {Get the Message}
 Buffer:=GetMessageText(MESSAGE_ID_BANNER);
 {Center the Message}
 X:=((Viewport.X2 - Viewport.X1) - Length(Buffer)) div 2;
 Y:=1;
 {Show the Message}
 ConsoleWriteSlowMotion(Window,X,Y,Buffer,GetMessageDelay(MESSAGE_ID_BANNER));

 {Move down two lines}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetY(Window) + 2);
 
 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_WELCOME));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_WELCOME));
 
 {Destroy the Window}
 ConsoleWindowDestroy(Window);
 
 Result:=True;
end;

{==============================================================================}

function ShowIntroduction:Boolean;
var
 Window:TWindowHandle;
begin
 {}
 Result:=False;
 
 {Create a Window}
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_INTRO));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_INTRO));

 {Move to the bottom}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 2);
 
 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_INTRO_DONE));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_INTRO_DONE));
 
 {Destroy the Window}
 ConsoleWindowDestroy(Window);
 
 Result:=True;
end;
 
{==============================================================================}

function ShowBlinker:Boolean;
var
 State:Boolean;
 Count:LongWord;
 Window:TWindowHandle;
begin
 {}
 Result:=False;
 
 {Create a Window}
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_BLINKER));
 
 {Move down two lines}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetY(Window) + 2);
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_BLINKER));
 
 {Enable LED}
 ActivityLEDEnable;
 
 {Set State}
 State:=False;
 ConsoleWriteMessage(Window,'The LED is OFF');
 for Count:=1 to 10 do
  begin
   if State then
    begin
     {Turn Off LED}
     State:=False;
     ActivityLEDOff;
    
     {Move Cursor}
     ConsoleWindowSetX(Window,ConsoleWindowGetX(Window) - 3);
     
     {Update Message}
     ConsoleWriteMessage(Window,'OFF');
    end
   else
    begin
     {Turn On LED}
     State:=True;
     ActivityLEDOn;
     
     {Move Cursor}
     ConsoleWindowSetX(Window,ConsoleWindowGetX(Window) - 4);
     
     {Update Message}
     ConsoleWriteMessage(Window,'ON ');
    end;    
    
   {Wait}
   Sleep(GetMessageDelay(MESSAGE_ID_BLINKER));
  end;

 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_BLINKER));

 {Move to the bottom}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 2);
 
 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_BLINKER_DONE));
 
 {Wait} 
 Sleep(GetMessageDelay(MESSAGE_ID_BLINKER_DONE));
 
 {Destroy the Window}
 ConsoleWindowDestroy(Window);
 
 Result:=True;
end;
 
{==============================================================================}

function ShowConsole:Boolean;
var
 Count:LongWord;
 Window:TWindowHandle;
 TopRight:TWindowHandle;
 BottomLeft:TWindowHandle;
 BottomRight:TWindowHandle;
 Bottom:TWindowHandle;
 Right:TWindowHandle;
begin
 {}
 Result:=False;
 
 {Create a Window}
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_CONSOLE));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE));
 
 
 {Create Top Right}
 TopRight:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPRIGHT,True);
 
 {Show the Message}
 ConsoleWriteMessage(TopRight,GetMessageText(MESSAGE_ID_CONSOLE_TOP_RIGHT));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_TOP_RIGHT));


 {Create Bottom Left}
 BottomLeft:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);
 
 {Show the Message}
 ConsoleWriteMessage(BottomLeft,GetMessageText(MESSAGE_ID_CONSOLE_BOTTOM_LEFT));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_BOTTOM_LEFT));


 {Create Bottom Right}
 BottomRight:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMRIGHT,True);
 
 {Show the Message}
 ConsoleWriteMessage(BottomRight,GetMessageText(MESSAGE_ID_CONSOLE_BOTTOM_RIGHT));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_BOTTOM_RIGHT));

 
 {Remove Top Right}
 ConsoleWindowDestroy(TopRight);
 {Remove Bottom Left}
 ConsoleWindowDestroy(BottomLeft);
 {Remove Bottom Right}
 ConsoleWindowDestroy(BottomRight);
 
 
 {Create Bottom}
 Bottom:=ConsoleWindowCreateEx(ConsoleDeviceGetDefault,INVALID_HANDLE_VALUE,SizeOf(TConsoleWindow),WINDOW_STATE_INVISIBLE,WINDOW_MODE_TEXT,CONSOLE_POSITION_BOTTOM,True);
 
 {Set Colors}
 ConsoleWindowSetForecolor(Bottom,COLOR_WHITE);
 ConsoleWindowSetBackcolor(Bottom,COLOR_BLACK);

 {Show Bottom}
 ConsoleWindowShow(Bottom);
 
 {Show the Message}
 ConsoleWriteMessage(Bottom,GetMessageText(MESSAGE_ID_CONSOLE_BOTTOM));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_BOTTOM));
 
 {Remove Bottom}
 ConsoleWindowDestroy(Bottom);
 
 
 {Create Right}
 Right:=ConsoleWindowCreateEx(ConsoleDeviceGetDefault,INVALID_HANDLE_VALUE,SizeOf(TConsoleWindow),WINDOW_STATE_INVISIBLE,WINDOW_MODE_TEXT,CONSOLE_POSITION_RIGHT,True);
 
 {Set Colors}
 ConsoleWindowSetForecolor(Right,COLOR_ORANGE);
 ConsoleWindowSetBackcolor(Right,COLOR_MIDGRAY);

 {Show Right}
 ConsoleWindowShow(Right);
 
 {Show the Message}
 ConsoleWriteMessage(Right,GetMessageText(MESSAGE_ID_CONSOLE_RIGHT));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_RIGHT));
 
 {Remove Right}
 ConsoleWindowDestroy(Right);
 
 
 {Create Right again}
 Right:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,True);
 
 {Show the Message}
 ConsoleWriteMessage(Right,GetMessageText(MESSAGE_ID_CONSOLE_SCROLLUP));

 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_SCROLLUP));

 {Fill the Window}
 while ConsoleWindowGetY(Right) < ConsoleWindowGetRows(Right) - 1 do
  begin
   {Show the Message}
   ConsoleWriteMessage(Right,GetMessageText(MESSAGE_ID_CONSOLE_SCROLLUP));
  end;

 {Update the Position}
 ConsoleWindowSetX(Right,1);
 ConsoleWindowSetY(Right,ConsoleWindowGetRows(Right) - 1);
  
 {Scroll the Window Up}
 for Count:=1 to ConsoleWindowGetRows(Right) do
  begin
   ConsoleWindowWriteLn(Right,'');
   Sleep(DEMO_SCROLL_DELAY);
  end;

 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_SCROLLUP));
 

 
 {Reset the Position}
 ConsoleWindowSetX(Right,1);
 ConsoleWindowSetY(Right,1);

 {Show the Message}
 ConsoleWriteMessage(Right,GetMessageText(MESSAGE_ID_CONSOLE_SCROLLDOWN));
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_SCROLLDOWN));
 {Fill the Window}
 while ConsoleWindowGetY(Right) < ConsoleWindowGetRows(Right) - 1 do
  begin
   {Show the Message}
   ConsoleWriteMessage(Right,GetMessageText(MESSAGE_ID_CONSOLE_SCROLLDOWN));
  end;

 {Update the Position}
 ConsoleWindowSetX(Right,1);
 ConsoleWindowSetY(Right,1);
  
 {Scroll the Window Down}
 for Count:=1 to ConsoleWindowGetRows(Right) do
  begin
   ConsoleWindowScrollDown(Right,1,1);
   Sleep(DEMO_SCROLL_DELAY);
  end;

 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_SCROLLDOWN));
 
 {Remove Right again}
 ConsoleWindowDestroy(Right);
 
 
 {Move to the bottom}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 2);
 
 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_CONSOLE_DONE));
 
 {Wait} 
 Sleep(GetMessageDelay(MESSAGE_ID_CONSOLE_DONE));
 
 {Destroy the Window}
 ConsoleWindowDestroy(Window);
 
 Result:=True;
end;

{==============================================================================}

function ShowThreads:Boolean;
var
 Y:LongWord;
 Delay:LongWord;
 Window:TWindowHandle;
 ThreadWindow:TWindowHandle;
 UtilizationWindow:TWindowHandle;
 Thread1:TDemoThread;
 Thread2:TDemoThread;
 UtilizationThread:TUtilizationThread;
begin
 {}
 Result:=False;
 
 {Create a Window}
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_THREADS));
 
 {Move down two lines}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetY(Window) + 2);
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_THREADS));

 {Check CPU Count}
 if CPUGetCount > 1 then
  begin
   {Create Utilization Window}
   UtilizationWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);
   
   {Create Utilization Thread}
   UtilizationThread:=TUtilizationThread.Create(UtilizationWindow);
  end;
 
 {Save current row}
 Y:=ConsoleWindowGetY(Window);
 
 {Show current threads}
 ConsoleWriteMessage(Window,'There are currently ' + IntToStr(ThreadGetCount) + ' threads');
 ConsoleWindowWriteLn(Window,'');
 
 {Show current memory}
 ConsoleWriteMessage(Window,'And ' + GetBoardUsedMemory + ' of memory is in use');
 ConsoleWindowWriteLn(Window,'');
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_THREADS_COUNT));

 
 {Create Thread Window}
 ThreadWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,True);
 
 {Show creating thread 1}
 ConsoleWriteMessage(ThreadWindow,'Creating the first thread to write messages on the console');
 ConsoleWindowWriteLn(ThreadWindow,'');
 
 {Create thread 1}
 Thread1:=TDemoThread.Create(ThreadWindow);

 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_THREADS_CREATE));

 {Show creating thread 2}
 ConsoleWriteMessage(ThreadWindow,'Creating the second thread');
 ConsoleWindowWriteLn(ThreadWindow,'');
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_THREADS_CREATE));
 
 {Create thread 2}
 Thread2:=TDemoThread.Create(ThreadWindow);
 
 {Start thread 1}
 Thread1.Start;

 {Start thread 2}
 Thread2.Start;
 
 
 {Get delay}
 Delay:=GetMessageDelay(MESSAGE_ID_THREADS_RUN);
 while Delay > 0 do
  begin
   {Restore postition}
   ConsoleWindowSetX(Window,1);
   ConsoleWindowSetY(Window,Y);
   
   {Show current threads}
   ConsoleWriteMessage(Window,'There are currently ' + IntToStr(ThreadGetCount) + ' threads');
   ConsoleWindowWriteLn(Window,'');
   
   {Show current memory}
   ConsoleWriteMessage(Window,'And ' + GetBoardUsedMemory + ' of memory is in use');
   ConsoleWindowWriteLn(Window,'');
   
   {Decrement}
   if Delay > DEMO_REFRESH_DELAY then
    begin
     Dec(Delay,DEMO_REFRESH_DELAY);
    end
   else
    begin
     Delay:=0;
    end;    
   
   {Wait}
   Sleep(DEMO_REFRESH_DELAY); 
  end;
 
 
 {Destroy thread 1}
 Thread1.Terminate;
 {Wait}
 WaitForThreadTerminate(Thread1.ThreadId,0);
 Thread1.Free;
 
 {Destroy thread 2}
 Thread2.Terminate;
 {Wait}
 WaitForThreadTerminate(Thread2.ThreadId,0);
 Thread2.Free;

 {Restore postition}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,Y);
 
 {Show current threads}
 ConsoleWriteMessage(Window,'There are currently ' + IntToStr(ThreadGetCount) + ' threads');
 ConsoleWindowWriteLn(Window,'');
 
 {Show current memory}
 ConsoleWriteMessage(Window,'And ' + GetBoardUsedMemory + ' of memory is in use');
 ConsoleWindowWriteLn(Window,'');
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_THREADS_DESTROY));
 
 {Destroy Thread Window}
 ConsoleWindowDestroy(ThreadWindow);
 
 
 {Move to the bottom}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 2);
 
 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_THREADS_DONE));
 
 {Wait} 
 Sleep(GetMessageDelay(MESSAGE_ID_THREADS_DONE));
 
 {Check CPU Count}
 if CPUGetCount > 1 then
  begin
   {Destroy Utilization Thread}
   UtilizationThread.Terminate;
   
   {Window destroyed by Thread}
  end;
  
 {Destroy the Window}
 ConsoleWindowDestroy(Window);
 
 Result:=True;
end;

{==============================================================================}

function ShowDateTime:Boolean;
var
 Window:TWindowHandle;
 SecondWindow:TWindowHandle;
begin
 {}
 Result:=False;
 
 {Create a Window}
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_DATETIME));

 {Move down two lines}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetY(Window) + 2);
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_DATETIME));


 {Create Second Window}
 SecondWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,True);
 
 {Show date time examples}
 ConsoleWindowWriteLn(SecondWindow,'Here are some example date and time formats:');
 ConsoleWindowWriteLn(SecondWindow,'');
 
 ConsoleWindowWriteLn(SecondWindow,' Time is ' + TimeToStr(Time));
 ConsoleWindowWriteLn(SecondWindow,' Date is ' + DateToStr(Date));
 ConsoleWindowWriteLn(SecondWindow,' Short date and time is ' + DateTimeToStr(Now));
 ConsoleWindowWriteLn(SecondWindow,' Long date and time is ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now));
 ConsoleWindowWriteLn(SecondWindow,'');
 ConsoleWindowWriteLn(SecondWindow,'You can also use your own custom formatting for exactly the result you need.');
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_DATETIME_FORMAT));
 
 {Show timezone examples}
 ConsoleWindowWriteLn(SecondWindow,'Timezones are supported as well, here''s a quick trip round the world.');
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {Australia}
 SetCurrentTimezone('AUS Eastern Standard Time');
 ConsoleWindowWriteLn(SecondWindow,'It is ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now) + ' in Melbourne, Australia');
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {China}
 SetCurrentTimezone('China Standard Time');
 ConsoleWindowWriteLn(SecondWindow,'      ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now) + ' in Beijing, China');
 ConsoleWindowWriteLn(SecondWindow,'');

 {USA}
 SetCurrentTimezone('Eastern Standard Time');
 ConsoleWindowWriteLn(SecondWindow,'      ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now) + ' in New York, USA');
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {New Zealand}
 SetCurrentTimezone('New Zealand Standard Time');
 ConsoleWindowWriteLn(SecondWindow,'      ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now) + ' in Auckland, New Zealand');
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {France}
 SetCurrentTimezone('Romance Standard Time');
 ConsoleWindowWriteLn(SecondWindow,'      ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now) + ' in Paris, France');
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {GMT}
 SetCurrentTimezone('GMT Standard Time');
 ConsoleWindowWriteLn(SecondWindow,'  and ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now) + ' in London, England');
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_DATETIME_TIMEZONE));
 
 {Destroy Second Window}
 ConsoleWindowDestroy(SecondWindow);

 
 {Move to the bottom}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 2);
 
 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_DATETIME_DONE));
 
 {Wait} 
 Sleep(GetMessageDelay(MESSAGE_ID_DATETIME_DONE));
 
 {Destroy the Window}
 ConsoleWindowDestroy(Window);
 
 Result:=True;
end;
 
{==============================================================================}

function ShowFiles:Boolean;
var
 Count:Integer;
 Filename:String;
 SearchRec:TSearchRec;
 StringList:TStringList;
 FileStream:TFileStream;
 Window:TWindowHandle;
 SecondWindow:TWindowHandle;
begin
 {}
 Result:=False;
 
 {Create a Window}
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_FILES));

 {Move down two lines}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetY(Window) + 2);
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_FILES));
 

 {Create Second Window}
 SecondWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_RIGHT,True);
 
 {Display contents}
 ConsoleWindowWriteLn(SecondWindow,'Ultibo uses drive letters by default, here''s whats on C:\');
 ConsoleWindowWriteLn(SecondWindow,'');
 if FindFirst('C:\*.*',faAnyFile,SearchRec) = 0 then
  begin
   repeat
    {Print the file found to the screen}
    ConsoleWindowWriteLn(SecondWindow,' File name is ' + SearchRec.Name + ', Size is ' + IntToStr(SearchRec.Size) + ', Time is ' + DateTimeToStr(FileDateToDateTime(SearchRec.Time)));

   until FindNext(SearchRec) <> 0;
  end;
 FindClose(SearchRec);
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_FILES_DIR));
 
 
 {Create files}
 ConsoleWindowWriteLn(SecondWindow,'Of course the full range of functions is supported for creating, deleting, renaming etc');
 ConsoleWindowWriteLn(SecondWindow,'');
 ConsoleWindowWriteLn(SecondWindow,' Creating folder "C:\My Files"');
 if not DirectoryExists('C:\My Files') then
  begin
   CreateDir('C:\My Files');
  end;
 ConsoleWindowWriteLn(SecondWindow,' Creating folder "C:\My Files\Secret"'); 
 if not DirectoryExists('C:\My Files\Secret') then
  begin
   CreateDir('C:\My Files\Secret');
  end;
 Filename:='C:\My Files\Secret\What is the secret.txt';
 ConsoleWindowWriteLn(SecondWindow,' Creating file "' + Filename + '"'); 
 if not FileExists(Filename) then
  begin
   try
    FileStream:=TFileStream.Create(Filename,fmCreate);
    
    StringList:=TStringList.Create;
    StringList.Add('>There is no secret!');
    StringList.Add('>');
    StringList.Add('>All you need is imagination and a bit of effort.');
    StringList.Add('>');
    
    StringList.SaveToStream(FileStream);
    
    FileStream.Free;
    StringList.Free;
   except
    ConsoleWindowWriteLn(SecondWindow,' Hmmm, that''s embarrassing. Couldn''t create the file due to an error"'); 
   end;
  end;
 ConsoleWindowWriteLn(SecondWindow,'');
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_FILES_CREATE));
  
 {Display file}
 ConsoleWindowWriteLn(SecondWindow,'So what is the secret?'); 
 ConsoleWindowWriteLn(SecondWindow,'');
 ConsoleWindowWriteLn(SecondWindow,' Contents of file "' + Filename + '"');
 ConsoleWindowWriteLn(SecondWindow,'');
 if FileExists(Filename) then
  begin
   try
    FileStream:=TFileStream.Create(Filename,fmOpenReadWrite);
    
    StringList:=TStringList.Create;
    StringList.LoadFromStream(FileStream);
    
    for Count:=0 to StringList.Count - 1 do
     begin
      ConsoleWindowWriteLn(SecondWindow,StringList.Strings[Count]);
     end;
    
    FileStream.Free;
    StringList.Free;
   except
    ConsoleWindowWriteLn(SecondWindow,' Oops, there was an error opening the file. Sorry"'); 
   end;
  end; 
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_FILES_TYPE));
 
 {Destroy Second Window}
 ConsoleWindowDestroy(SecondWindow);
 
 
 {Move to the bottom}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 2);
 
 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_FILES_DONE));
 
 {Wait} 
 Sleep(GetMessageDelay(MESSAGE_ID_FILES_DONE));
 
 {Destroy the Window}
 ConsoleWindowDestroy(Window);
 
 Result:=True;
end;

{==============================================================================}

function ShowCompleted(var Rerun:Boolean):Boolean;
var
 X:LongWord;
 Y:LongWord;
 Count:LongWord;
 Buffer:String;
 Window:TWindowHandle;
 UtilizationWindow:TWindowHandle;
begin
 {}
 Result:=False;
 
 {Create a Window}
 Window:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_TOPLEFT,True);

 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_COMPLETED));

 {Move to 6 lines from the bottom}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 6);
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_COMPLETED));
 
 
 {Get the Message}
 Buffer:=GetMessageText(MESSAGE_ID_ULTIBO);
 {Center the Message}
 X:=(ConsoleWindowGetCols(Window) - Length(Buffer)) div 2;
 Y:=ConsoleWindowGetY(Window);
 {Show the Message}
 ConsoleWriteSlowMotion(Window,X,Y,Buffer,GetMessageDelay(MESSAGE_ID_ULTIBO));
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_ULTIBO_ORG));

 
 {Move down one line}
 ConsoleWindowSetX(Window,X);
 ConsoleWindowSetY(Window,ConsoleWindowGetY(Window) + 1);
 
 {Get the Message}
 Buffer:=GetMessageText(MESSAGE_ID_ULTIBO_ORG);
 {Center the Message}
 X:=(ConsoleWindowGetCols(Window) - Length(Buffer)) div 2;
 Y:=ConsoleWindowGetY(Window);
 {Show the Message}
 ConsoleWriteMessage(Window,Buffer);
 
 {Wait}
 Sleep(GetMessageDelay(MESSAGE_ID_ULTIBO_ORG));
 
 
 {Move to the bottom}
 ConsoleWindowSetX(Window,1);
 ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 2);
 
 {Show the Message}
 ConsoleWriteMessage(Window,GetMessageText(MESSAGE_ID_COMPLETED_DONE));
 
 if KeyboardGetCount > 0 then
  begin
   {Check Keypressed}
   Rerun:=GetKeyboardKeypressed(GetMessageDelay(MESSAGE_ID_COMPLETED_DONE));
   
   {Move to the bottom}
   ConsoleWindowSetX(Window,1);
   ConsoleWindowSetY(Window,ConsoleWindowGetRows(Window) - 2);
   
   for Count:=1 to 3 do
    begin
     {Scroll the last line off the bottom}
     ConsoleWindowScrollDown(Window,ConsoleWindowGetRows(Window) - 2,1);
    end; 
  end
 else
  begin 
   {Wait}
   Sleep(GetMessageDelay(MESSAGE_ID_COMPLETED_DONE));
  end; 
 
 {Don't destroy the window unless re running}
 if Rerun then
  begin
   ConsoleWindowDestroy(Window);
  end
 else
  begin
   {Create Utilization Window}
   UtilizationWindow:=ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_BOTTOMLEFT,True);
   
   {Create Utilization Thread}
   TUtilizationThread.Create(UtilizationWindow);
  end;  

 Result:=True;
end;

{==============================================================================}
{==============================================================================}

function GetMessageText(Id:LongWord):String;
begin
 {}
 Result:='';
 
 case Id of
  MESSAGE_ID_NONE:begin
    Result:='';
   end; 
  MESSAGE_ID_BANNER:begin
    Result:='Welcome to Ultibo...';
   end; 
  MESSAGE_ID_WELCOME:begin
    Result:='This demonstration will show some of the features and capabilities of Ultibo core, the entire demo takes about 3 minutes'
            + ' and then you can explore other features further on your own. We''ll cover common features like the console, memory, threading,'
            + ' dates and times as well as files and folders. Then we''ll move on to have a look at the network if you have one connected and some'
            + ' devices like USB.';
   end;
  MESSAGE_ID_INTRO:begin
    Result:='First some basics, this is a ' + GetBoardDescription + ' with ' + GetBoardTotalMemory  + ' of memory and ' + GetBoardCPUs + ' available. ' + MESSAGE_DOUBLEEND
            + ' The current temperature of the system on chip is about ' + GetBoardTemperature + ' degrees. ' + MESSAGE_LINEEND
            + ' The clock speed of the CPU is ' + GetBoardCPUClock + '. ' + MESSAGE_LINEEND
            + ' After loading this demo there is ' + GetBoardAvailableMemory + ' of available memory remaining.';
   end;   
  MESSAGE_ID_INTRO_DONE:begin
    Result:='Let''s move on with the demo, we''ll start with something simple...';
   end;   
  MESSAGE_ID_BLINKER:begin
    Result:='By using standard GPIO functions turning an LED on or off is quite simple, we can use the activity LED on the board to demonstrate. ' + MESSAGE_DOUBLEEND;
   end;   
  MESSAGE_ID_BLINKER_DONE:begin
    Result:='Enough of that, let''s look at the console...';
   end;   
  MESSAGE_ID_CONSOLE:begin
    Result:='Console output might not be important for many uses but you never know when you''ll need it. ' + MESSAGE_DOUBLEEND
                             + ' Windows can appear in multiple places, this one is top left.';
   end;   
  MESSAGE_ID_CONSOLE_TOP_RIGHT:begin
    Result:='This one is top right.';
   end;   
  MESSAGE_ID_CONSOLE_BOTTOM_LEFT:begin
    Result:='This one is bottom left.';
   end;   
  MESSAGE_ID_CONSOLE_BOTTOM_RIGHT:begin
    Result:='And this one is bottom right.';
   end;   
  MESSAGE_ID_CONSOLE_BOTTOM:begin
    Result:='They can also be different colors, this one is white on black.';
   end;   
  MESSAGE_ID_CONSOLE_RIGHT:begin
    Result:='And this one is orange on grey.';
   end;   
  MESSAGE_ID_CONSOLE_SCROLLUP:begin
    Result:='Text can scroll up the screen. ';
   end;   
  MESSAGE_ID_CONSOLE_SCROLLDOWN:begin
    Result:='Or it can scroll down the screen instead. ';
   end;   
  MESSAGE_ID_CONSOLE_DONE:begin
    Result:='Next, what can we do with threads...';
   end;   
  MESSAGE_ID_THREADS:begin
    Result:='Ultibo core doesn''t have processes so threads are used for everything. There aren''t really any limits you can just keep creating threads'
            + ' until you run out of memory. ' + MESSAGE_DOUBLEEND
            + ' Let''s create a couple of threads to scribble on a console window and we''ll get them to use some memory at the same time. ';
    if CPUGetCount > 1 then
     begin
      Result:=Result  + MESSAGE_DOUBLEEND + ' Because there are multiple CPUs available we should also show the utilization of them while this is happening.';
     end;     
   end;   
  MESSAGE_ID_THREADS_COUNT:begin
    Result:=''; {Nothing}
   end;   
  MESSAGE_ID_THREADS_CREATE:begin
    Result:=''; {Nothing}
   end;   
  MESSAGE_ID_THREADS_RUN:begin
    Result:=''; {Nothing}
   end;   
  MESSAGE_ID_THREADS_DESTROY:begin
    Result:=''; {Nothing}
   end;   
  MESSAGE_ID_THREADS_DONE:begin
    if ClockGetTime > TIME_TICKS_TO_1970 then
     begin
      Result:='Look at the time, I really must be going. Date and time...';
     end
    else
     begin
      Result:='The clock isn''t set, maybe the network is not connected. We''ll skip time and move on...';
     end;
   end;   
  MESSAGE_ID_DATETIME:begin
    Result:='There are a full range of standard date and time functions as well as timezone support with daylight savings adjustment. ' + MESSAGE_DOUBLEEND
            + ' The current date and time is ' + FormatDateTime(DefaultFormatSettings.LongDateFormat + ' ' + DefaultFormatSettings.LongTimeFormat,Now);
   end;  
  MESSAGE_ID_DATETIME_FORMAT:begin
    Result:=''; {Nothing}
   end;
  MESSAGE_ID_DATETIME_TIMEZONE:begin
    Result:=''; {Nothing}
   end;  
  MESSAGE_ID_DATETIME_DONE:begin
    Result:='It''s sometimes good to be able to save some information on disk. Files next...';
   end;   
  MESSAGE_ID_FILES:begin
    Result:='The filesystem comes with support for FAT, NTFS and CDFS out of the box, but the filesystem is modular so many more can be added as needed.';
   end;   
  MESSAGE_ID_FILES_DIR:begin
    Result:=''; {Nothing}
   end;
  MESSAGE_ID_FILES_CREATE:begin
    Result:=''; {Nothing}
   end;
  MESSAGE_ID_FILES_TYPE:begin
    Result:=''; {Nothing}
   end;
  MESSAGE_ID_FILES_DONE:begin
    Result:='There''s much more than that but I think you get the idea...';
   end;   
  MESSAGE_ID_COMPLETED:begin
    Result:='That''s all for the demo, but it isn''t the end because you can continue to explore some other features on your own. ';
    if GetNetworkConnected then
     begin
      Result:=Result  + MESSAGE_DOUBLEEND + ' The network is connected so if you point a browser at http://' + GetNetworkAddress + ' you can see some '
              + ' of the inner workings of Ultibo core. ' + MESSAGE_DOUBLEEND
              + ' If you have a telnet client like PuTTY you can telnet to ' + GetNetworkAddress + ' and explore the shell commands like copying files and '
              + ' restarting the system.';
     end
    else
     begin
      Result:=Result + MESSAGE_DOUBLEEND + ' There''s no network available at the moment but I''ll leave you with a console shell so you can explore the shell '
              + ' commands like copying files and restarting the system.';
     end;     
   end;   
  MESSAGE_ID_COMPLETED_DONE:begin
    if KeyboardGetCount > 0 then
     begin
      Result:='Since you have a keyboard connected, you can press any key within 10 seconds to start the demo again.';
     end
    else
     begin    
      Result:='';
     end; 
   end;   
  MESSAGE_ID_ULTIBO:begin
    Result:='Ultibo.org | Make something amazing';
   end;   
  MESSAGE_ID_ULTIBO_ORG:begin
    Result:='https://ultibo.org';
   end;   
 end;
end;

{==============================================================================}

function GetMessageDelay(Id:LongWord):LongWord;
begin
 {}
 Result:=0;
 
 case Id of
  MESSAGE_ID_NONE:Result:=0;
  MESSAGE_ID_BANNER:Result:=200; {Slow motion delay not message delay}
  MESSAGE_ID_WELCOME:Result:=25000;
  MESSAGE_ID_INTRO:Result:=15000;
  MESSAGE_ID_INTRO_DONE:Result:=4000;
  MESSAGE_ID_BLINKER:Result:=1000;
  MESSAGE_ID_BLINKER_DONE:Result:=4000;
  MESSAGE_ID_CONSOLE:Result:=4000;
  MESSAGE_ID_CONSOLE_TOP_RIGHT:Result:=1500;
  MESSAGE_ID_CONSOLE_BOTTOM_LEFT:Result:=1500;
  MESSAGE_ID_CONSOLE_BOTTOM_RIGHT:Result:=2500;
  MESSAGE_ID_CONSOLE_BOTTOM:Result:=2500;
  MESSAGE_ID_CONSOLE_RIGHT:Result:=2500;
  MESSAGE_ID_CONSOLE_SCROLLUP:Result:=2000;
  MESSAGE_ID_CONSOLE_SCROLLDOWN:Result:=2000;
  MESSAGE_ID_CONSOLE_DONE:Result:=4000;
  MESSAGE_ID_THREADS:Result:=5000;
  MESSAGE_ID_THREADS_COUNT:Result:=1000;
  MESSAGE_ID_THREADS_CREATE:Result:=1000;
  MESSAGE_ID_THREADS_RUN:begin
    if CPUGetCount > 1 then
     begin
      Result:=15000;
     end
    else
     begin
      Result:=11000;
     end;     
   end;
  MESSAGE_ID_THREADS_DESTROY:Result:=4000;
  MESSAGE_ID_THREADS_DONE:Result:=6000;
  MESSAGE_ID_DATETIME:Result:=5000;
  MESSAGE_ID_DATETIME_FORMAT:Result:=5000;
  MESSAGE_ID_DATETIME_TIMEZONE:Result:=8000;
  MESSAGE_ID_DATETIME_DONE:Result:=6000;
  MESSAGE_ID_FILES:Result:=5000;
  MESSAGE_ID_FILES_DIR:Result:=5000;
  MESSAGE_ID_FILES_CREATE:Result:=5000;
  MESSAGE_ID_FILES_TYPE:Result:=8000;
  MESSAGE_ID_FILES_DONE:Result:=6000;
  MESSAGE_ID_COMPLETED:Result:=8000;
  MESSAGE_ID_COMPLETED_DONE:begin
    if KeyboardGetCount > 0 then
     begin
      Result:=11000;
     end
    else
     begin    
      Result:=4000;
     end; 
   end;  
  MESSAGE_ID_ULTIBO:Result:=200; {Slow motion delay not message delay}
  MESSAGE_ID_ULTIBO_ORG:Result:=500;
 end;
end;

{==============================================================================}

function GetBoardDescription:String;
var
 BoardType:LongWord;
begin
 {}
 Result:='';
 
 {Get Board Type}
 BoardType:=BoardGetType;
 
 case BoardType of
  BOARD_TYPE_RPIA:Result:='Raspberry Pi A';
  BOARD_TYPE_RPIB:Result:='Raspberry Pi B';
  BOARD_TYPE_RPI_COMPUTE:Result:='Raspberry Pi Compute Module';
  BOARD_TYPE_RPIA_PLUS:Result:='Raspberry Pi A+';
  BOARD_TYPE_RPIB_PLUS:Result:='Raspberry Pi B+';
  BOARD_TYPE_RPI2B:Result:='Raspberry Pi 2B';
  BOARD_TYPE_RPI_ZERO:Result:='Raspberry Pi Zero';
  BOARD_TYPE_RPI3B:Result:='Raspberry Pi 3B';
  BOARD_TYPE_QEMUVPB:Result:='QEMU Versatile PB';
  BOARD_TYPE_RPI_COMPUTE3:Result:='Raspberry Pi Compute Module 3';
  BOARD_TYPE_RPI_ZERO_W:Result:='Raspberry Pi Zero W';
 end;
end;

{==============================================================================}

function GetBoardTotalMemory:String;
var
 MemorySize:LongWord;
begin
 {}
 Result:='';
 
 {Get Memory Size}
 MemorySize:=MemoryGetSize;
 
 Result:=IntToStr(MemorySize div SIZE_1M) + 'MB';
end;

{==============================================================================}

function GetBoardAvailableMemory:String;
var
 HeapStatus:TFPCHeapStatus;
begin
 {}
 Result:='';
 
 {Get Heap Status}
 HeapStatus:=GetFPCHeapStatus;
 
 Result:=IntToStr(HeapStatus.CurrHeapFree div SIZE_1M) + 'MB';
end;
 
{==============================================================================}

function GetBoardUsedMemory:String;
var
 HeapStatus:TFPCHeapStatus;
begin
 {}
 Result:='';
 
 {Get Heap Status}
 HeapStatus:=GetFPCHeapStatus;
 
 Result:=IntToStr(HeapStatus.CurrHeapUsed div SIZE_1M) + 'MB';
end;
 
{==============================================================================}

function GetBoardCPUs:String;
var
 CPUCount:LongWord;
begin
 {}
 Result:='';
 
 {Get CPU Count}
 CPUCount:=CPUGetCount;
 
 if CPUCount > 1 then
  begin
   Result:=IntToStr(CPUCount) + ' CPUs';
  end
 else
  begin
   Result:=IntToStr(CPUCount) + ' CPU';
  end;  
end;

{==============================================================================}

function GetBoardTemperature:String;
var
 Temperature:LongWord;
begin
 {}
 Result:='';
 
 {Get Temperature}
 Temperature:=TemperatureGetCurrent(TEMPERATURE_ID_SOC);
 
 Result:=IntToStr(Temperature div 1000);
end;

{==============================================================================}

function GetBoardCPUClock:String;
var
 Clock:LongWord;
begin
 Result:='';
 
 {Get Clock}
 Clock:=ClockGetRate(CLOCK_ID_CPU);
 
 Result:=IntToStr(Clock div 1000000) + 'MHz';
end;

{==============================================================================}

function GetNetworkAddress:String;
var
 Winsock2TCPClient:TWinsock2TCPClient;
begin
 {}
 Result:='';
 
 {Create TCP Client}
 Winsock2TCPClient:=TWinsock2TCPClient.Create;
 
 {Get the Address}
 Result:=Winsock2TCPClient.LocalAddress;
 
 {Destroy the Client}
 Winsock2TCPClient.Free;
end;
 
{==============================================================================}

function GetNetworkConnected:Boolean;
var
 Address:String;
begin
 {}
 Result:=False;
 
 {Get Address}
 Address:=GetNetworkAddress;
 
 {Check Address}
 if Address = '' then Exit;
 if Address = '0.0.0.0' then Exit;
 if Address = '255.255.255.255' then Exit;
 if Copy(Address,1,Length('192.168.100.')) = '192.168.100.' then Exit;
 
 Result:=True;
end;

{==============================================================================}

function GetKeyboardKeypressed(Timeout:LongWord):Boolean;
var
 Ch:Char;
begin
 {}
 Result:=False;
 
 {Clear Buffer}
 while ConsolePeekKey(Ch,nil) do
  begin
   ConsoleGetKey(Ch,nil);
  end;
 
 {Wait Buffer}
 while not ConsolePeekKey(Ch,nil) do
  begin
   {Check Timeout}
   if Timeout >= DEMO_KEYPRESS_DELAY then
    begin
     Dec(Timeout,DEMO_KEYPRESS_DELAY);
    end
   else
    begin
     Timeout:=0;
    end;    
   if Timeout = 0 then Exit;
   
   {Wait}
   Sleep(DEMO_KEYPRESS_DELAY);
  end;
 
 {Get Key}
 ConsoleGetKey(Ch,nil);
 
 Result:=True;
end;

{==============================================================================}

function ConsoleWriteMessage(Handle:TWindowHandle;const Text:String):Boolean;
var
 X:LongWord;
 Next:String;
 Buffer:String;
 Cols:LongWord;
begin
 {}
 Result:=False;
 
 {Get Current Columns}
 Cols:=ConsoleWindowGetCols(Handle);
 
 {Display the Text}
 Buffer:=Text;
 while Length(Buffer) <> 0 do
  begin
   Next:=GetFirstWord(Buffer,MESSAGE_DELIMITER);
   
   {Check word}
   if Next = MESSAGE_LINEEND then
    begin
     {Move to the next line}
     ConsoleWindowWriteLn(Handle,'');
    end
   else
    begin   
     {Get Current Column}
     X:=ConsoleWindowGetX(Handle);
     if (X > 1) and ((X + Length(Next)) >= Cols) then
      begin
       {Move to the next line}
       ConsoleWindowWriteLn(Handle,'');
       
       {Write the word}
       ConsoleWindowWrite(Handle,Next + MESSAGE_DELIMITER);
      end
     else
      begin
       {Write the word}
       ConsoleWindowWrite(Handle,Next + MESSAGE_DELIMITER);
      end;    
    end;  
  end;
end;

{==============================================================================}

function ConsoleWriteSlowMotion(Handle:TWindowHandle;X,Y:LongWord;const Text:String;Delay:LongWord):Boolean;
var
 Count:LongWord;
begin
 {}
 Result:=False;
 
 for Count:=1 to Length(Text) do
  begin
   {Write the next character}
   ConsoleWindowWriteChrEx(Handle,Text[Count],X + (Count - 1),Y,ConsoleWindowGetForecolor(Handle),ConsoleWindowGetBackcolor(Handle));

   {Wait}
   Sleep(Delay);
  end;
  
 Result:=True; 
end;
   
{==============================================================================}
{==============================================================================}

initialization 
 {Disable the console shell}
 CONSOLE_SHELL_ENABLED:=False;
 
 {Disable console logging}
 CONSOLE_REGISTER_LOGGING:=False;
 
   
{==============================================================================}
{==============================================================================}

end.
     
