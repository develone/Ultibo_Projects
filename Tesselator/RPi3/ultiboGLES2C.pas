unit ultiboGLES2C;

{$mode delphi} {Default to Delphi compatible syntax}
{$H+}          {Default to AnsiString}
{$inline on}   {Allow use of Inline procedures}
 
interface

{The VideoCore IV GPU supports both OpenGL ES 1.1 and 2.0 so we have enabled the Free Pascal units
 for both versions in Ultibo. When using OpenGL ES 2.0 the GLES20 unit includes the EGL functions
 needed to setup and initialize OpenGL ES so the only additional requirement is DispmanX}
uses GlobalConst,GlobalTypes,GlobalConfig,Platform,HeapManager,Console,SysUtils,GLES20,Threads,DispmanX,VC4,Math,Syscalls,Mouse,DWCOTG;


{$linklib libultiboCgeneric}

procedure ultibo_C_main; cdecl; external 'libultiboCgeneric' name 'ultibo_C_main';

procedure glSwapBuffer;export; cdecl;
procedure getMouseXY(var cx: integer; var cy: integer; var btc: integer);export; cdecl;
procedure getScreenSize(var scrWidth: LongWord ;  var scrHeight: LongWord);export; cdecl;
procedure getKey(var value : integer); export; cdecl;


type
 {A structure to keep track of the overall state including the DispmanX and EGL handles}
 {GLES2 State}
 PGLES2State = ^TGLES2State;
 TGLES2State = record
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
  Alpha:VC_DISPMANX_ALPHA_T;
  NativeWindow:EGL_DISPMANX_WINDOW_T;
  ConfigAttributes:array[0..18] of EGLint;
  ContextAttributes:array[0..2] of EGLint;
 end;



 var
  State:TGLES2State;
  Thread1Handle:TThreadHandle;
  mouseCX: integer = 0;
  mouseCY: integer = 0;
  mouseBt: integer = 0;


{The main functions of our GLES2 example} 
procedure StartGLES2;
procedure CleanupGLES2(var State:TGLES2State);
procedure NonBlockingMouse;
 
{Functions dealing with initializing, updating and rendering our OpenGL ES scene}
function GLES2Init(var State:TGLES2State):Boolean;


implementation
procedure getKey(var value : integer);
var
  Key: Char;
begin
  // Check if a key is available (without waiting)
  if ConsolePeekKey(Key, nil) then
  begin
    // Remove the key from the buffer
    ConsoleGetKey(Key, nil);

    // Return the ordinal value of the character
    value := Ord(Key);
  end
  else
    value := -1;
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

procedure StartGLES2;
{var}
 {State:TGLES2State;}
 {Scene:TGLES2Scene;}
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
 FillChar(State,SizeOf(TGLES2State),0);
 
 {Initialize the OpenGL ES 2.0 state}
 GLES2Init(State);
 
 {Initialize the OpenGL ES 2.0 scene}


 {Empty the keyboard buffer before starting our loop}
 while ConsoleKeypressed do
  begin
   ConsoleReadKey;
  end;
         Thread1Handle:=BeginThread(@NonBlockingMouse,nil,Thread1Handle,THREAD_STACK_DEFAULT_SIZE);
         if Thread1Handle = INVALID_HANDLE_VALUE then
           begin
            {If the thread handle is not valid then BeginThread failed}
            {ConsoleWindowWriteLn(WindowHandle,'Failed to create Thread1');}
           end
          else
           begin
            ultibo_C_main;
           end;



 while True do
  begin
   {Render the scene and display again}
   {GLES2RenderScene(State,Scene); original}
   
   {Check for a keypress and break if found}
   if ConsoleKeyPressed then Break;
  end;
  
 {Cleanup the OpenGL ES 2.0 state and scene} 
 CleanupGLES2(State);
 
 {Deinitialize the VC4}
 BCMHostDeinit;
end;

procedure CleanupGLES2(var State:TGLES2State);
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

function GLES2Init(var State:TGLES2State):Boolean;
var
 Config:EGLConfig;
 ConfigCount:EGLint;
 EGLResult:EGLBoolean;

 DestRect:VC_RECT_T;
 SourceRect:VC_RECT_T;
 DispmanUpdate:DISPMANX_UPDATE_HANDLE_T;
begin
 {}
 {This function provides a good general outline of how to initialize OpenGL ES 2.0 on the
  VideoCore IV and obtain the necessary handles and contexts.
  
  You will need something similar to this in all OpenGL ES applications before you can
  perform any OpenGL ES operations. 
  
  For an OpenGL ES 1.1 version of this function please see the HelloGLES example}
 Result:=False;
 
 {Setup some DispmanX and EGL defaults}
 State.DispmanDisplay:=DISPMANX_NO_HANDLE;
 State.DispmanElement:=DISPMANX_NO_HANDLE;
 DispmanUpdate:=DISPMANX_NO_HANDLE;
 State.Display:=EGL_NO_DISPLAY;
 State.Surface:=EGL_NO_SURFACE;
 State.Context:=EGL_NO_CONTEXT;
 
 {Setup the alpha channel state}
 State.Alpha.flags:= DISPMANX_FLAGS_ALPHA_FIXED_ALL_PIXELS;
 State.Alpha.opacity:=255;
 State.Alpha.mask:=0;
 
 {Setup the EGL configuration attributes}
 State.ConfigAttributes[0]:=EGL_RENDERABLE_TYPE;
 State.ConfigAttributes[1]:=EGL_OPENGL_ES2_BIT;
 State.ConfigAttributes[2]:=EGL_SURFACE_TYPE;
 State.ConfigAttributes[3]:=EGL_WINDOW_BIT;
 State.ConfigAttributes[4]:=EGL_BLUE_SIZE;
 State.ConfigAttributes[5]:=8;
 State.ConfigAttributes[6]:=EGL_GREEN_SIZE;
 State.ConfigAttributes[7]:=8;
 State.ConfigAttributes[8]:=EGL_RED_SIZE;
 State.ConfigAttributes[9]:=8;
 State.ConfigAttributes[10]:=EGL_STENCIL_SIZE;
 State.ConfigAttributes[11]:=8;
 {State.ConfigAttributes[12]:=EGL_ALPHA_SIZE;}
 {State.ConfigAttributes[13]:=8;}
 State.ConfigAttributes[12]:=EGL_DEPTH_SIZE;
 State.ConfigAttributes[13]:=24;
 State.ConfigAttributes[14]:=EGL_SAMPLE_BUFFERS;
 State.ConfigAttributes[15]:=1;
 State.ConfigAttributes[16]:=EGL_SAMPLES;
 State.ConfigAttributes[17]:=4;
 State.ConfigAttributes[18]:=EGL_NONE;
 
 {Setup the EGL context attributes}
 State.ContextAttributes[0]:=EGL_CONTEXT_CLIENT_VERSION;
 State.ContextAttributes[1]:=2;
 State.ContextAttributes[2]:=EGL_NONE;
 
 try
  {Get an EGL display connection}
  State.Display:=eglGetDisplay(EGL_DEFAULT_DISPLAY);
  if State.Display = EGL_NO_DISPLAY then Exit;
  
  {Initialize the EGL display connection}
  EGLResult:=eglInitialize(State.Display,nil,nil);
  if EGLResult = EGL_FALSE then Exit;
  
  {Get an appropriate EGL framebuffer configuration}
  EGLResult:=eglChooseConfig(State.Display,@State.ConfigAttributes,@Config,1,@ConfigCount);
  if EGLResult = EGL_FALSE then Exit;
  
  {Bind to the OpenGL ES API}
  EGLResult:=eglBindAPI(EGL_OPENGL_ES_API);  {EGL_OPENVG_API  EGL_OPENGL_ES_API}
  if EGLResult = EGL_FALSE then Exit;
  
  {Create an EGL rendering context}
  State.Context:=eglCreateContext(State.Display,Config,EGL_NO_CONTEXT,@State.ContextAttributes);
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
  State.DispmanElement:=vc_dispmanx_element_add(DispmanUpdate,State.DispmanDisplay,0 {Layer},@DestRect,0 {Source},@SourceRect,DISPMANX_PROTECTION_NONE,@State.Alpha,nil {Clamp},DISPMANX_NO_ROTATE {Transform});
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
  
  {Preserve the buffers on swap}
  EGLResult:=eglSurfaceAttrib(State.Display,State.Surface,EGL_SWAP_BEHAVIOR,EGL_BUFFER_DESTROYED ); {EGL_BUFFER_PRESERVED}
  if EGLResult = EGL_FALSE then Exit;
 
  {Connect the EGL context to the EGL surface}
  EGLResult:=eglMakeCurrent(State.Display,State.Surface,State.Surface,State.Context);
  if EGLResult = EGL_FALSE then Exit;
  
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
