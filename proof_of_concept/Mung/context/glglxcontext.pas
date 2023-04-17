unit GLGLXContext;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF LCLGTK2}
  gtk2proc, gtk2, gdk2, gdk2x, gtk2def,
  {$ENDIF}
  {$IFDEF LCLGTK}
  gtkproc, gtk, gtkdef, gdk,
  {$ENDIF}
  {$IFDEF LCLQT}
  QT4, QTWidgets,
  {$ENDIF}
  Classes, SysUtils, GLContext, dglOpenGL, XUtil, LCLType, xlib, LCLIntf;

type
  TGLGLXContext = class (TGLContext)
  private
    FDisplay: PDisplay;
    FDC: GLXDrawable;
    FRC: GLXContext;
    function GetRealHandle(AHandle: HWND): HWND; inline;
  public
    constructor Create(AHandle: HWND; MajorVersion, MinorVersion: Integer; ForwardCompatible: Boolean = true; Options: TGLCOptions = [glcDoubleBuffered]; ColorBits: Integer = 32; DepthBits: Integer = 24; StencilBits: Integer = 0; AccumBits: Integer = 0; AuxBuffers: Integer = 0; Layer: Integer = 0; ASharedContext: TGLContext = nil); override;
    destructor Destroy; override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure SwapBuffers; override;
  end;

implementation

type
  TGLXFBConfigs = array [0..0] of GLXFBConfig;
  PGLXFBConfigS = ^TGLXFBConfigS;

{%REGION TGLGLXContext}

constructor TGLGLXContext.Create(AHandle: HWND; MajorVersion, MinorVersion: Integer; ForwardCompatible: Boolean = true; Options: TGLCOptions = [glcDoubleBuffered]; ColorBits: Integer = 32; DepthBits: Integer = 24; StencilBits: Integer = 0; AccumBits: Integer = 0; AuxBuffers: Integer = 0; Layer: Integer = 0; ASharedContext: TGLContext = nil);
var
  vi            : PXVisualInfo;
  AIAttribs     : array [0..36] of GLInt;
  ACurrentScreen: Integer;
  AfbConfigs    : PGLXFBConfigs;
  AfbConfigCount: GLInt;
begin
  inherited Create(AHandle,MajorVersion,MinorVersion,ForwardCompatible,Options,ColorBits,DepthBits,StencilBits,AccumBits,AuxBuffers,Layer,ASharedContext);
  //Open DEFAULT Display
  FDisplay:=XOpenDisplay(nil);
  FDC:=GetRealHandle(AHandle);//GetDC(AHandle);

  AIAttribs[0]:=GLX_X_RENDERABLE;      AIAttribs[1]:=GL_TRUE;
  AIAttribs[2]:=GLX_DRAWABLE_TYPE;     AIAttribs[3]:=GLX_WINDOW_BIT;
  AIAttribs[4]:=GLX_RENDER_TYPE;       AIAttribs[5]:=GLX_RGBA_BIT;
  AIAttribs[6]:=GLX_X_VISUAL_TYPE;     AIAttribs[7]:=GLX_TRUE_COLOR;
  AIAttribs[8]:=GLX_RED_SIZE;          AIAttribs[9]:=8;
  AIAttribs[10]:=GLX_GREEN_SIZE;       AIAttribs[11]:=8;
  AIAttribs[12]:=GLX_BLUE_SIZE;        AIAttribs[13]:=8;
  if ColorBits=32 then begin
    AIAttribs[14]:=GLX_ALPHA_SIZE;       AIAttribs[15]:=8;
  end else begin
    AIAttribs[14]:=GLX_ALPHA_SIZE;       AIAttribs[15]:=0;
  end;
  AIAttribs[16]:=GLX_DEPTH_SIZE;       AIAttribs[17]:=DepthBits;
  AIAttribs[18]:=GLX_STENCIL_SIZE;     AIAttribs[19]:=StencilBits;
  //AIAttribs[20]:=GLX_DOUBLEBUFFER;     AIAttribs[21]:=GL_TRUE;
  AIAttribs[20]:=GLX_AUX_BUFFERS;      AIAttribs[21]:=AuxBuffers;
  AIAttribs[22]:=GLX_DOUBLEBUFFER;     AIAttribs[23]:=BoolToGL(glcDoubleBuffered in Options);
  AIAttribs[24]:=GLX_STEREO;           AIAttribs[25]:=BoolToGL(glcStereo in Options);
  AIAttribs[26]:=GLX_LEVEL;            AIAttribs[27]:=Layer;
  case AccumBits of
    24: begin
        AIAttribs[28]:=GLX_ACCUM_RED_SIZE;   AIAttribs[29]:=8;
        AIAttribs[30]:=GLX_ACCUM_GREEN_SIZE; AIAttribs[31]:=8;
        AIAttribs[32]:=GLX_ACCUM_BLUE_SIZE;  AIAttribs[33]:=8;
        AIAttribs[34]:=GLX_ACCUM_ALPHA_SIZE; AIAttribs[35]:=0;
      end;
    32: begin
        AIAttribs[28]:=GLX_ACCUM_RED_SIZE;   AIAttribs[29]:=8;
        AIAttribs[30]:=GLX_ACCUM_GREEN_SIZE; AIAttribs[31]:=8;
        AIAttribs[32]:=GLX_ACCUM_BLUE_SIZE;  AIAttribs[33]:=8;
        AIAttribs[34]:=GLX_ACCUM_ALPHA_SIZE; AIAttribs[35]:=8;
      end;
    else begin
        AIAttribs[28]:=GLX_ACCUM_RED_SIZE;   AIAttribs[29]:=0;
        AIAttribs[30]:=GLX_ACCUM_GREEN_SIZE; AIAttribs[31]:=0;
        AIAttribs[32]:=GLX_ACCUM_BLUE_SIZE;  AIAttribs[33]:=0;
        AIAttribs[34]:=GLX_ACCUM_ALPHA_SIZE; AIAttribs[35]:=0;
      end;
  end;
  AIAttribs[36]:=0;

  ACurrentScreen:=XDefaultScreen(FDisplay);
  //AfbConfigCount is a return parameter: it's not necesary to set it before!
  AfbConfigs:=glXChooseFBConfig(FDisplay, ACurrentScreen, @AiAttribs[0], @AfbConfigCount);
  if AfbConfigs = nil then raise EGLContext.Create('Invalid Attributes');
  if AfbConfigCount<=0 then raise EGLContext.Create('no fbConfig available');

  vi:=glXGetVisualFromFBConfig(FDisplay,AfbConfigs^[0]);
  //vi:=glXChooseVisual(FDisplay,ACurrentScreen,@AiAttribs[0]);
  if vi=nil then raise EGLContext.Create('Invalid Visual');

  if ASharedContext is TGLGLXContext
    then FRC:=glxCreateNewContext(FDisplay,AfbConfigs^[0],GLX_RGBA_TYPE,TGLGLXContext(ASharedContext).FRC,true)
    else FRC:=glxCreateNewContext(FDisplay,AfbConfigs^[0],GLX_RGBA_TYPE,nil,true);

  {
  if ASharedContext is TGLGLXContext
    then FRC:=glxCreateContext(FDisplay,vi,TGLGLXContext(ASharedContext).FRC,false)
    else FRC:=glxCreateContext(FDisplay,vi,nil,false);
  }
  if FRC=nil then raise EGLContext.Create('No Context received');
end;

destructor TGLGLXContext.Destroy;
begin
  glxMakeContextCurrent(FDisplay,0,0,nil);
  glxDestroyContext(FDisplay,FRC);
  inherited Destroy;
end;

procedure TGLGLXContext.Activate;
begin
  if not glxMakeContextCurrent(FDisplay,FDC,FDC,FRC)
    then raise EGLContext.Create('Context activation failed');
end;

procedure TGLGLXContext.Deactivate;
begin
  glXMakeContextCurrent(FDisplay,0,0,nil);
end;

procedure TGLGLXContext.SwapBuffers;
begin
  glXSwapBuffers(FDisplay,FDC);
end;

function TGLGLXContext.GetRealHandle(AHandle: HWND): HWND; inline;
{$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK)}
var
  AvGTKWidget: PGTKWidget;
  Aptr       : Pointer;
{$ENDIF}
begin
{$IF DEFINED(LCLGTK2) or DEFINED(LCLGTK)}
  with TGtkDeviceContext(AHandle)
  do if Assigned(Widget)
    then Aptr:=Pointer(Widget)
    else Aptr:=Pointer(AHandle);
  AvGTKWidget:=GetFixedWidget(Aptr);
  // Dirty workaround: force realize
  gtk_widget_realize(AvGTKWidget);
  {$IFDEF LCLGTK2}
    gtk_widget_set_double_buffered(AvGTKWidget, False);
    Result:=GDK_WINDOW_XWINDOW(PGdkDrawable(AvGTKWidget^.window));
  {$ENDIF}
  {$IFDEF LCLGTK}
    Result:=GDK_WINDOW_XWINDOW(PGdkWindowPrivate(AvGTKWidget^.window));
  {$ENDIF}
{$ELSE}
  {$IFDEF LCLQT}
    Result:=QWidget_winId(TQTWidget(AHandle).widget);
  {$ELSE}
    {$MESSAGE Warn glx not availible for this widget}
  {$ENDIF}
{$ENDIF}
end;

{%ENDREGION}

end.

