unit GLCanvasUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType, MInterfacedObject, PluginType, MCanvasUnit,
  PictureType, OpenGLContext, GL, GLU, glext, GraphX32, AdvFunc, GR32,
  SharedCanvasUnit, AdvThreads, CanvasType, VisType2;

type
  TGLCScreenBuffer= record
    TexCreated      : Boolean;
    RecreateTexPic  : Boolean;
    Tex             : GLuint;
  end;

  TGLSubCanvas    = class (TMCanvas, IMCanvas)
  private
    FCam             : TGLMatrix;
    FHalfWidth       : Double;
    FHalfHeight      : Double;
    cube_rotationx   : GLFloat;
    cube_rotationy   : GLFloat;
    cube_rotationz   : GLFloat;
    FLastScreen      : TGLCScreenBuffer;
    FIsFrontBuffer   : Boolean;
    FHeight          : Integer;
    FWidth           : Integer;
    FLineWidth       : Real;
  protected
    function GetHeight: CvFloat; stdcall; override;
    function GetWidth: CvFloat; stdcall; override;
    function GetLastScreenTransformationMatrix: PGLMatrix; cdecl;
    procedure BindPic(APic: IMPicture); inline;
    procedure Paint;

    property Height: Integer read FHeight write FHeight;
    property Width: Integer read FWidth write FWidth;
  public
    constructor Create(AWidth,AHeight: Integer);
    destructor Destroy; override;

    //procedure Reset(AFrameStarted: TEmptyEvent); override;
    procedure FrameStart; override;
    procedure FrameEnd; override;
    procedure Resized; override;

    procedure Line(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall; override;
    procedure FillRect(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall; override;
    procedure Vanish(const Color: CvColor); stdcall; override;
    procedure Rotate(const Phi: CvFloat; const Color: CvColor); stdcall; override;
    procedure Zoom(const X,Y: CvFloat; const Color: CvColor); stdcall; override;
    procedure Move(const X,Y: CvFloat); stdcall; override;
    procedure Clear(const Color: CvColor); stdcall; override;
    procedure Draw(X,Y: MVSInt; Pic: IMPicture); stdcall; override;
    procedure StretchDraw(X1,Y1,X2,Y2: MVSInt; Pic: IMPicture); stdcall; override;
    procedure RectDraw(const SrcRect,DestRect: MVRect; Pic: IMPicture); stdcall; override;

    property LastScreenTransformationMatrix: PGLMatrix read GetLastScreenTransformationMatrix;
    property LineWidth: Real read FLineWidth write FLineWidth;
  end;

  TGLCanvas       = class (TGLSubCanvas, IPGL2DCanvas)
  private
    FGLControl   : TOpenGLControl;
    FFrameStarted: Boolean;
    procedure FGLControlResize(Sender: TObject);
    procedure FGLControlPaint(Sender: TObject);
  protected
    procedure Reset(AFrameStarted: TEmptyEvent); override;
    procedure FrameStart; override;
    procedure FrameEnd; override;
    function GetWidth: Double; cdecl;
    function GetHeight: Double; cdecl;
    function GetType: TPCanvasType; cdecl;
  public
    constructor Create(AGLControl: TOpenGlControl);
    destructor Destroy; override;
  end;

const
   glcX = 0;
   glcY = 1;
   glcZ = 2;
   glcW = 3;

implementation

{%REGION TGLSubCanvas}

constructor TGLSubCanvas.Create(AWidth,AHeight: Integer);
var
  I,J: Integer;
begin
  inherited Create;
  FLineWidth:=1.0;
  FWidth:=AWidth;
  AHeight:=AHeight;
  for I:=0 to 3 do for J:=0 to 3 do begin
    if I<>J
      then FCam[I][J]:=0.0
      else FCam[I][J]:=1.0;
  end;

  //per definition by double-buffering zuerst der backbuffer
  //bei single-buffering der frontbuffer;
  FIsFrontBuffer:=false;
  FLastScreen.RecreateTexPic:=true;
  FLastScreen.TexCreated:=false;
end;

destructor TGLSubCanvas.Destroy;
begin
  inherited Destroy;
end;

{procedure TGLSubCanvas.Reset(AFrameStarted: TEmptyEvent);
begin
  inherited Reset(AFrameStarted)
end;}

procedure TGLSubCanvas.Paint;
var
  I,J         : Integer;
  ATexMem     : Pointer;
  ATexMemCount: Integer;
begin
  glDisable(GL_DEPTH_TEST);
  glEnable(GL_TEXTURE_2D);
  glDisable(GL_BLEND);
  glDisable(GL_ALPHA_TEST);
  glEnable(GL_POINT_SMOOTH);
  glEnable(GL_LINE_SMOOTH);

  if not FLastScreen.TexCreated then begin
    glGenTextures(1, @FLastScreen.Tex);
    FLastScreen.TexCreated:=true;
  end;

  if FLastScreen.RecreateTexPic then begin
    glBindTexture(GL_TEXTURE_2D, FLastScreen.Tex);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D,GL_GENERATE_MIPMAP,GL_TRUE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    ATexMemCount:=NextPowerOfTwo(Round(Width))*NextPowerOfTwo(Round(Height));
    GetMem(ATexMem,ATexMemCount*3);
    Graphx32.SetRGBColors(ATexMem^,ATexMemCount,$FF000000);

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, NextPowerOfTwo(Round(Width)), NextPowerOfTwo(Round(Height)),0, GL_RGB, GL_UNSIGNED_BYTE, ATexMem);

    FreeMem(ATexMem,ATexMemCount*3);

    glBindTexture(GL_TEXTURE_2D,0);
    FLastScreen.RecreateTexPic:=false;
  end;
  //Render
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  //gluPerspective(45.0, double(width) / height, 0.1, 100.0);
  gluOrtho2D(-FHalfWidth,FHalfWidth,FHalfHeight,-FHalfHeight);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glLoadMatrixd(@FCam);

  for I:=0 to 3 do for J:=0 to 3 do begin
    if I<>J
      then FCam[I][J]:=0.0
      else FCam[I][J]:=1.0;
  end;

  glClearColor(0.0,0.0,0.0,1.0);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLineWidth(FLineWidth);
  glBindTexture(GL_TEXTURE_2D,FLastScreen.Tex);
  glBegin(GL_QUADS);
          glTexCoord2f(0.0,1.0);
          glVertex2f(-FHalfWidth,-FHalfHeight);
          glTexCoord2f(1.0,1.0);
          glVertex2f(FHalfWidth,-FHalfHeight);
          glTexCoord2f(1.0,0.0);
          glVertex2f(FHalfWidth,FHalfHeight);
          glTexCoord2f(0.0,0.0);
          glVertex2f(-FHalfWidth,FHalfHeight);
  glEnd();
  glBindTexture(GL_TEXTURE_2D,0);

  glEnable(GL_BLEND);
  glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);
  glEnable(GL_ALPHA_TEST);
  inherited FrameStart;
end;

procedure TGLSubCanvas.FrameStart;
begin
  Paint;
end;

procedure TGLSubCanvas.FrameEnd;
begin
  glDisable(GL_BLEND);
  glDisable(GL_ALPHA_TEST);
  //sinn auch noch nicht genau verstanden, andernfalls
  //werden aber nur die zuletzt benutzten Farbkanäle
  //in die textur geschrieben
  glColor4ub($FF,$FF,$FF,$FF);
  glBindTexture(GL_TEXTURE_2D,FLastScreen.Tex);
  glCopyTexImage2D(GL_TEXTURE_2D,0,GL_RGB8,0,0,Round(Width),Round(Height),0);
  FIsFrontBuffer:=not FIsFrontBuffer;
  glBindTexture(GL_TEXTURE_2D,0);
end;

procedure TGLSubCanvas.Resized;
begin
  FHalfWidth:=Width/2.0;
  FHalfHeight:=Height/2.0;
  FLastScreen.RecreateTexPic:=true;
end;

function TGLSubCanvas.GetHeight: CvFloat; stdcall;
begin
  Result:=FHeight;
end;

function TGLSubCanvas.GetWidth: CvFloat; stdcall;
begin
  Result:=FWidth;
end;

function TGLSubCanvas.GetLastScreenTransformationMatrix: PGLMatrix; cdecl;
begin
  Result:=@FCam;
end;

procedure TGLSubCanvas.Line(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall;
begin
  glBegin(GL_LINES);
          glColor4ub((Color shr 16) and $FF,(Color shr 8) and $FF,Color and $FF,Color shr 24);                              // Set The Color To Violet
          glVertex2f(X1-FHalfWidth,Y1-FHalfHeight);
          glVertex2f(X2-FHalfWidth,Y2-FHalfHeight);
  glEnd();
end;

procedure TGLSubCanvas.FillRect(const X1,Y1,X2,Y2: CvFloat; const Color: CvColor); stdcall;
begin
  glBegin(GL_QUADS);
          glColor4ub((Color shr 16) and $FF,(Color shr 8) and $FF,Color and $FF,Color shr 24);                              // Set The Color To Violet
          glVertex2f(X1-FHalfWidth,Y1-FHalfHeight);
          glVertex2f(X2-FHalfWidth,Y1-FHalfHeight);
          glVertex2f(X2-FHalfWidth,Y2-FHalfHeight);
          glVertex2f(X1-FHalfWidth,Y2-FHalfHeight);
  glEnd();
end;

procedure TGLSubCanvas.Vanish(const Color: CvColor); stdcall;
begin
  glBegin(GL_QUADS);
          glColor4ub((Color shr 16) and $FF,(Color shr 8) and $FF,Color and $FF,Color shr 24);
          glVertex2f(-FHalfWidth,-FHalfHeight);
          glVertex2f(FHalfWidth,-FHalfHeight);
          glVertex2f(FHalfWidth,FHalfHeight);
          glVertex2f(-FHalfWidth,FHalfHeight);
  glEnd();
end;

procedure TGLSubCanvas.Rotate(const Phi: CvFloat; const Color: CvColor); stdcall;
begin
  glPushMatrix;
  glLoadMatrixd(@FCam);
  glRotatef(Phi,0.0,0.0,-1.0);
  glGetDoublev(GL_MODELVIEW_MATRIX,@FCam);
  glPopMatrix;
end;

procedure TGLSubCanvas.Zoom(const X,Y: CvFloat; const Color: CvColor); stdcall;
begin
  glPushMatrix;
  glLoadMatrixd(@FCam);
  glScalef(X,Y,0.0);
  glGetDoublev(GL_MODELVIEW_MATRIX,@FCam);
  glPopMatrix;
end;

procedure TGLSubCanvas.Move(const X,Y: CvFloat); stdcall;
begin
  glPushMatrix;
  glLoadMatrixd(@FCam);
  glTranslatef(X,Y,0.0);
  glGetDoublev(GL_MODELVIEW_MATRIX,@FCam);
  glPopMatrix;
end;

procedure TGLSubCanvas.Clear(const Color: CvColor); stdcall;
begin
  glClearColor(((Color shr 16) and $FF)/glfloat(255.0),((Color shr 8) and $FF)/glfloat(255.0),(Color and $FF)/glfloat(255.0),1.0{(Color shr 24)/glfloat(255.0)});
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
end;

procedure TGLSubCanvas.BindPic(APic: IMPicture); inline;
begin
  with PPictureContent(APic.IDP)^ do begin
    if TexID=0 then glGenTextures(1,@TexID);
    glBindTexture(GL_TEXTURE_2D, TexID);
    if Changed then begin
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      //glTexParameteri(GL_TEXTURE_2D,GL_GENERATE_MIPMAP,GL_TRUE);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
      glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
      glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

      //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, NextPowerOfTwo(Round(Width)), NextPowerOfTwo(Round(Height)),0, GL_RGB, GL_UNSIGNED_BYTE, ATexMem);
      gluBuild2DMipmaps(GL_TEXTURE_2D,4,Bitmap.Width,Bitmap.Height,GL_BGRA,GL_UNSIGNED_BYTE,Bitmap.Bits);
      Changed:=false;
    end;
  end;
end;

procedure TGLSubCanvas.Draw(X,Y: MVSInt; Pic: IMPicture); stdcall;
var
  AWidth,AHeight: Integer;
begin
  BindPic(Pic);
  with PPictureContent(Pic.IDP)^ do begin
    AWidth:=Bitmap.Width;
    AHeight:=Bitmap.Height;
  end;
  glBegin(GL_QUADS);
          glColor4ub($FF,$FF,$FF,$FF);
          glTexCoord2f(0.0,0.0);
          glVertex2f(-FHalfWidth+X,-FHalfHeight+Y);
          glTexCoord2f(1.0,0.0);
          glVertex2f(-FHalfWidth+X+AWidth,-FHalfHeight+Y);
          glTexCoord2f(1.0,1.0);
          glVertex2f(-FHalfWidth+X+AWidth,-FHalfHeight+Y+AHeight);
          glTexCoord2f(0.0,1.0);
          glVertex2f(-FHalfWidth+X,-FHalfHeight+Y+AHeight);
  glEnd();
  glBindTexture(GL_TEXTURE_2D,0);
end;

procedure TGLSubCanvas.StretchDraw(X1,Y1,X2,Y2: MVSInt; Pic: IMPicture); stdcall;
var
  AWidth,AHeight: Integer;
begin
  BindPic(Pic);
  with PPictureContent(Pic.IDP)^ do begin
    AWidth:=Bitmap.Width;
    AHeight:=Bitmap.Height;
  end;
  glBegin(GL_QUADS);
          glColor4ub($FF,$FF,$FF,$FF);
          glTexCoord2f(0.0,0.0);
          glVertex2f(-FHalfWidth+X1,-FHalfHeight+Y1);
          glTexCoord2f(1.0,0.0);
          glVertex2f(-FHalfWidth+X2,-FHalfHeight+Y1);
          glTexCoord2f(1.0,1.0);
          glVertex2f(-FHalfWidth+X2,-FHalfHeight+Y2);
          glTexCoord2f(0.0,1.0);
          glVertex2f(-FHalfWidth+X1,-FHalfHeight+Y2);
  glEnd();
  glBindTexture(GL_TEXTURE_2D,0);
{var
  ABmp: TBitmap32;}
//begin
  {ABmp:=TBitmap32(Pic.IDP);
  FBitmap.Draw(Rect(X1,Y1,X2,Y2),Rect(0,0,ABmp.Width,ABmp.Height),ABmp);}
end;

procedure TGLSubCanvas.RectDraw(const SrcRect,DestRect: MVRect; Pic: IMPicture); stdcall;
{var
  ABmp: TBitmap32;}
begin
{  ABmp:=TBitmap32(Pic.IDP);
  FBitmap.Draw(DestRect,SrcRect,ABmp);}
end;

{%ENDREGION}
{%REGION TGLCanvas}

constructor TGLCanvas.Create(AGLControl: TOpenGlControl);
begin
  inherited Create(AGLControl.Width,AGLControl.Height);
  FFrameStarted:=false;
  FGLControl:=AGLControl;
  AGLControl.OnPaint:=@FGLControlPaint;
  AGLControl.OnResize:=@FGLControlResize;
end;

destructor TGLCanvas.Destroy;
begin
  inherited Destroy;
end;

procedure TGLCanvas.FGLControlResize(Sender: TObject);
begin
  Width:=FGLControl.Width;
  Height:=FGLControl.Height;
  Resized;
end;

procedure TGLCanvas.FGLControlPaint(Sender: TObject);
begin
  if FFrameStarted then begin
    FFrameStarted:=false;
    Paint;
  end;
end;

procedure TGLCanvas.Reset(AFrameStarted: TEmptyEvent);
begin
  inherited Reset(AFrameStarted);
end;

procedure TGLCanvas.FrameStart;
var
  AMakeCurrent,ARestore: Boolean;
  AStr: string;
begin
  FFrameStarted:=true;
  FGLControl.Invalidate;
  //FGLControl.Paint;
  {FGLControl.
  AMakeCurrent:=FGLControl.MakeCurrent(true);
  //FGLControl.is
  if AMakeCurrent then begin
    FGLControl.Paint;
    ARestore:=FGLControl.RestoreOldOpenGLControl;
  end;}
end;

procedure TGLCanvas.FrameEnd;
begin
  inherited FrameEnd;
  FGLControl.SwapBuffers;
end;

function TGLCanvas.GetWidth: Double; cdecl;
begin
  Result:=FWidth;
end;

function TGLCanvas.GetHeight: Double; cdecl;
begin
  Result:=FHeight;
end;

function TGLCanvas.GetType: TPCanvasType; cdecl;
begin
  Result:=cGL2DCanvas;
end;

{%ENDREGION}

end.

