unit GLESUnit;

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}

{This example is directly ported from the hello triangle example in the hello_pi
 collection included with Raspbian. The original source can be found here:
 
 https://github.com/raspberrypi/userland/blob/master/host_applications/linux/apps/hello_pi/hello_triangle
 
 and if you compare it against this you will notice that most things are very similar.
 
 This means that you should be able to port many OpenGL ES examples to Ultibo with
 only minimal changes to account for the different environment.
 
 It is not the purpose of this example to teach you how to use OpenGL ES, for that
 you should consult one of the many online references and tutorials which cover
 everything from the very basic fundamentals to highly advanced topics.
 
 You can find the official Khronos reference for OpenGL ES 1.1 here:
 
 https://www.khronos.org/registry/OpenGL-Refpages/es1.1/xhtml/

} 

interface

{The VideoCore IV GPU supports both OpenGL ES 1.1 and 2.0 so we have enabled the Free Pascal units
 for both versions in Ultibo. When using OpenGL ES 1.1 and including the GLES11 unit you will also
 need to include EGL in order to access the functions required to setup and initialize OpenGL ES}
uses GlobalConst,GlobalTypes,GlobalConfig,Console,Classes,SysUtils,EGL,GLES11,DispmanX,VC4,Math,uBitmap,Mouse,Threads;

{$linklib Testnanogl}
procedure test; cdecl; external 'libTestnanogl' name 'test';

procedure glSwapBuffer;export; cdecl;
procedure getMouseXY(var cx: integer; var cy: integer; var btc: integer);export; cdecl;
procedure getScreenSize(var scrWidth: LongWord ;  var scrHeight: LongWord);export; cdecl;
 

type
 {A structure to keep track of the overall state including the DispmanX and EGL handles}
 {GLES State}
 PGLESState = ^TGLESState;
 TGLESState = record
  {Screen dimensions}
  ScreenWidth:LongWord;
  ScreenHeight:LongWord;
  {DispmanX window}
  DispmanDisplay:DISPMANX_DISPLAY_HANDLE_T;
  DispmanElement:DISPMANX_ELEMENT_HANDLE_T;
  {EGL data}
  Display:EGLDisplay;
  Surface:EGLSurface;
  Context:EGLContext;
  {EGL config}
  NativeWindow:EGL_DISPMANX_WINDOW_T;
  AttributeList:array[0..12] of EGLint;
 end;


var

  State:TGLESState;
  Thread1Handle:TThreadHandle;
  mouseCX: integer = 0;
  mouseCY: integer = 0;
  mouseBt: integer = 0;

 procedure NonBlockingMouse;


 

{The main functions of our GLES example} 
procedure StartGLES;
procedure CleanupGLES(var State:TGLESState);

{Functions dealing with initializing, updating and rendering our OpenGL ES scene}
function GLESInit(var State:TGLESState):Boolean;


implementation

procedure NonBlockingMouse;
var
  Mousedata : TMouseData;
  count : LongWord;
  ScalingX:Double;
  ScalingY:Double;
  ScreenWidth:LongWord;
  ScreenHeight:LongWord;
begin
 ScreenWidth  := State.ScreenWidth;
 ScreenHeight := State.ScreenHeight;
 while True do
  begin
  {if (MousePeek() = ERROR_SUCCESS) then}

  if MouseRead(@MouseData,SizeOf(TMouseData),Count) = ERROR_SUCCESS then
   begin
    {We received a mouse message so let's process it to see what it contains.
    The TMouseData structure will give us an X an Y Offset as well as any buttons
    that are currently pressed.}

    {Check the buttons}
    if MouseData.Buttons = 0 then
     begin
      mouseBt:=0;
     end
    else
     begin
      if (MouseData.Buttons and (MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON)) = (MOUSE_LEFT_BUTTON or MOUSE_RIGHT_BUTTON) then
       begin
        mouseBt:=3;
       end
      else if (MouseData.Buttons and MOUSE_LEFT_BUTTON) = MOUSE_LEFT_BUTTON then
       begin
        mouseBt:=1;
       end
      else if (MouseData.Buttons and MOUSE_RIGHT_BUTTON) = MOUSE_RIGHT_BUTTON then
       begin
        mouseBt:=2;
       end
      else
       begin
        mouseBt:=4;
       end;
     end;
   end;

   {Now update our mouse tracking for cursor X and Y}
         {Check if the X value is absolute instead of relative}
         if (MouseData.Buttons and MOUSE_ABSOLUTE_X) = MOUSE_ABSOLUTE_X then
          begin
           {For absolute values the maximum X field allows us to scale
            the cursor X value relative to the size of our screen}
           ScalingX:=MouseData.MaximumX / ScreenWidth;
           if ScalingX <= 0 then ScalingX:=1.0;

           mouseCX:=Trunc(MouseData.OffsetX / ScalingX);
          end
         else
          begin
           mouseCX:=mouseCX + MouseData.OffsetX;
          end;
         if mouseCX < 0 then mouseCX:=0;
         if mouseCX > (ScreenWidth - 1) then mouseCX:=ScreenWidth - 1;

         {Check if the Y value is absolute}
         if (MouseData.Buttons and MOUSE_ABSOLUTE_Y) = MOUSE_ABSOLUTE_Y then
          begin
           {Use maximum Y to scale the Y value to the screen}
           ScalingY:=MouseData.MaximumY / ScreenHeight;
           if ScalingY <= 0 then ScalingY:=1.0;

           mouseCY:=Trunc(MouseData.OffsetY / ScalingY);
          end
         else
          begin
           mouseCY:=mouseCY + MouseData.OffsetY;
          end;
         if mouseCY < 0 then mouseCY:=0;
         if mouseCY > (ScreenHeight - 1) then mouseCY:=ScreenHeight - 1;

end;
end;

procedure glSwapBuffer;
begin
  eglSwapBuffers(State.Display,State.Surface);
end;

procedure getMouseXY(var cx: integer; var cy: integer; var btc: integer);
begin
 cx  := mouseCX;
 cy  := mouseCY;
 btc := mouseBt;
end;

procedure getScreenSize(var scrWidth: LongWord ;  var scrHeight: LongWord);
begin
 scrWidth  := State.ScreenWidth;
 scrHeight := State.ScreenHeight;
end;

procedure StartGLES;
{var}
 {State:TGLESState;}
 {Scene:TGLESScene;}
begin
 {}
 {This is the stating point for our OpenGL ES example, this routine sets up all of the
  required elements and then continually redraws our scene onto the screen until a key
  is pressed}
  
 {All applications using the VideoCore IV must call BCMHostInit before doing any other
  operations. This will initialize all of the supporting libraries and start the VCHIQ
  communication service. Applications should also call BCMHostDeinit when they no longer
  require any VC4 services}
 BCMHostInit;
 
 {Clear our state and scene}
 FillChar(State,SizeOf(TGLESState),0);
 {FillChar(Scene,SizeOf(TGLESScene),0);}
 
 {Initialize the OpenGL ES state}
 GLESInit(State);


 {Empty the keyboard buffer before starting our loop}
 while ConsoleKeypressed do
  begin
   ConsoleReadKey;
  end;

 Thread1Handle:=BeginThread(@NonBlockingMouse,nil,Thread1Handle,THREAD_STACK_DEFAULT_SIZE);
 if Thread1Handle = INVALID_HANDLE_VALUE then
   begin
    {If the thread handle is not valid then BeginThread failed}
    {Failed to create Thread1 nothing will be shown)}
   end
  else
   begin
    test; {main thread}
   end;
  
 {while True do}
  {begin}
   {Update our model and apply the next rotation}
   {GLESUpdateModel(Scene);}
   {Render the scene and display again}
   {GLESRedrawScene(State,Scene);}
   {Check for a keypress and break if found}
   {if ConsoleKeyPressed then Break;}
  {end;}
  
 {Cleanup the OpenGL ES state and scene} 
 CleanupGLES(State);
 
 {Deinitialize the VC4}
 BCMHostDeinit;
end;

procedure CleanupGLES(var State:TGLESState);
var
 Success:Integer;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}  
 {Clear the screen}
 glClear(GL_COLOR_BUFFER_BIT);
 eglSwapBuffers(State.Display,State.Surface);

 {Destroy the EGL surface}
 eglDestroySurface(State.Display,State.Surface);
 
 {Begin a DispmanX update}
 DispmanUpdate:=vc_dispmanx_update_start(0);
 
 {Remove the DispmanX element}
 Success:=vc_dispmanx_element_remove(DispmanUpdate,State.DispmanElement);
 if Success <> 0 then Exit; 

 {And submit the change to DispmanX}
 vc_dispmanx_update_submit_sync(DispmanUpdate);
 
 {Close the DispmanX display we opened earlier}
 Success:=vc_dispmanx_display_close(State.DispmanDisplay);
 if Success <> 0 then Exit; 

 {Release OpenGL resources and terminate EGL}
 eglMakeCurrent(State.Display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT);
 eglDestroyContext(State.Display,State.Context);
 eglTerminate(State.Display);
end;

function GLESInit(var State:TGLESState):Boolean;
var
 Config:EGLConfig;
 ConfigCount:EGLint;
 EGLResult:EGLBoolean;

 DestRect:VC_RECT_T;
 SourceRect:VC_RECT_T;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}
 {This function provides a good general outline of how to initialize OpenGL ES 1.1 on the
  VideoCore IV and obtain the necessary handles and contexts.
  
  You will need something similar to this in all OpenGL ES applications before you can
  perform any OpenGL ES operations. 
  
  For an OpenGL ES 2.0 version of this function please see the HelloGLES2 example}
 Result:=False;
 
 {Setup some DispmanX and EGL defaults}
 State.DispmanDisplay:=DISPMANX_NO_HANDLE;
 State.DispmanElement:=DISPMANX_NO_HANDLE;
 DispmanUpdate:=DISPMANX_NO_HANDLE;
 State.Display:=EGL_NO_DISPLAY;
 State.Surface:=EGL_NO_SURFACE;
 State.Context:=EGL_NO_CONTEXT;
 
 {Setup the EGL attribute list}
 State.AttributeList[0]:=EGL_RED_SIZE;
 State.AttributeList[1]:=8;
 State.AttributeList[2]:=EGL_GREEN_SIZE;
 State.AttributeList[3]:=8;
 State.AttributeList[4]:=EGL_BLUE_SIZE;
 State.AttributeList[5]:=8;
 State.AttributeList[6]:=EGL_ALPHA_SIZE;
 State.AttributeList[7]:=8;
 State.AttributeList[8]:=EGL_STENCIL_SIZE;
 State.AttributeList[9]:=8;
 State.AttributeList[10]:=EGL_SURFACE_TYPE;
 State.AttributeList[11]:=EGL_WINDOW_BIT;
 State.AttributeList[12]:=EGL_NONE;
 
 try
  {Get an EGL display connection}
  State.Display:=eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if State.Display = EGL_NO_DISPLAY then Exit;
  
  {Initialize the EGL display connection}
  EGLResult:=eglInitialize(State.Display,nil,nil);
  if EGLResult = EGL_FALSE then Exit;
  
  {Get an appropriate EGL framebuffer configuration}
  EGLResult:=eglChooseConfig(State.Display,@State.AttributeList,@Config,1,@ConfigCount);
  if EGLResult = EGL_FALSE then Exit;
  
  {Create an EGL rendering context}
  State.Context:=eglCreateContext(State.Display,Config,EGL_NO_CONTEXT,nil);
  if State.Context = EGL_NO_CONTEXT then Exit;
  
  {Create an EGL window surface}
  if BCMHostGraphicsGetDisplaySize(DISPMANX_ID_MAIN_LCD,State.ScreenWidth,State.ScreenHeight) < 0 then Exit;
  
  {Setup the DispmanX source and destination rectangles}
  vc_dispmanx_rect_set(@DestRect,0,0,State.ScreenWidth,State.ScreenHeight);
  vc_dispmanx_rect_set(@SourceRect,0 shl 16,0 shl 16,State.ScreenWidth shl 16,State.ScreenHeight shl 16);
  
  {Open the DispmanX display}
  State.DispmanDisplay:=vc_dispmanx_display_open(DISPMANX_ID_MAIN_LCD);
  if State.DispmanDisplay = DISPMANX_NO_HANDLE then Exit;
  
  {Start a DispmanX update}
  DispmanUpdate:=vc_dispmanx_update_start(0);
  if DispmanUpdate = DISPMANX_NO_HANDLE then Exit;
  
  {Add a DispmanX element for our display}
  State.DispmanElement:=vc_dispmanx_element_add(DispmanUpdate,State.DispmanDisplay,0 {Layer},@DestRect,0 {Source},@SourceRect,DISPMANX_PROTECTION_NONE,nil {Alpha},nil {Clamp},DISPMANX_NO_ROTATE {Transform});
  if State.DispmanElement = DISPMANX_NO_HANDLE then Exit;
  
  {Define an EGL DispmanX native window structure}
  State.NativeWindow.Element:=State.DispmanElement;
  State.NativeWindow.Width:=State.ScreenWidth;
  State.NativeWindow.Height:=State.ScreenHeight;
  
  {Submit the DispmanX update}
  vc_dispmanx_update_submit_sync(DispmanUpdate);
 
  {Create an EGL window surface}
  State.Surface:=eglCreateWindowSurface(State.Display,Config,@State.NativeWindow,nil);
  if State.Surface = EGL_NO_SURFACE then Exit;

  {Connect the EGL context to the EGL surface}
  EGLResult:=eglMakeCurrent(State.Display,State.Surface,State.Surface,State.Context);
  if EGLResult = EGL_FALSE then Exit;
      
  {The following items are specific to this example and may or may not apply to other applications}    
  {Set the background color and clear the buffers (Notice that alpha value is 0.0 which gives us a transparent background)}
  glClearColor(0.15,0.25,0.35,0.0);

  {Enable back face culling}
  glEnable(GL_CULL_FACE);
  
  {Set the matrix mode}
  glMatrixMode(GL_MODELVIEW);
  
  Result:=True;
 finally
  {Check if the initialization was successful, if not cleanup}
  if not Result then
   begin
    {Close the DispmanX display if opened}
    if State.DispmanDisplay <> DISPMANX_NO_HANDLE then vc_dispmanx_display_close(State.DispmanDisplay);
    
    {Check for an EGL display connection}
    if State.Display <> EGL_NO_DISPLAY then
     begin
      {Terminate EGL}
      eglMakeCurrent(State.Display,EGL_NO_SURFACE,EGL_NO_SURFACE,EGL_NO_CONTEXT);
      
      {Destroy the EGL surface}
      if State.Surface <> EGL_NO_SURFACE then eglDestroySurface(State.Display,State.Surface);
      
      {Destroy the EGL context}
      if State.Context <> EGL_NO_CONTEXT then eglDestroyContext(State.Display,State.Context);

      {Terminate the EGL display}
      eglTerminate(State.Display);
     end;
   end;
 end;
end;



end.
