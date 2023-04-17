unit PicVisUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags,
  SimpleVis, VisAddInput, ParamOp, Math, CanvasType, Graphics, GL, IntfGraphics,
  GraphType, GLU, GLext, Forms, SysUtils, VisualisationUtils;

type
  TPictureDrawer       = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FPath       : IPString;
    FCurrentPath: string;

    FTexID      : GLUInt;
    procedure DrawPic(X1, Y1, X2, Y2: GLFloat);
    procedure LoadPic(AFileName: string);
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

const
  VIDPICTUREDRAWER       : TGUID = '{2C75A06B-5A8D-471A-5069-634472617720}';
  VIDCANVAS              : TGUID = '{2C75A06B-5A8D-471A-4341-4E5641533031}';

  {
  PIDPICTUREDRAWER       : TGUID = '{4334A878-E5B7-49D8-9CFE-527F4B9E2D89}';
  PIDCANVAS              : TGUID = '{2D5EBA5B-A79E-4E91-8455-212B8AB650A2}';
  }

  PICTUREDRAWERPATHNAME  = 'Pfad';
  CANVASWIDTHNAME        = 'Breite';
  CANVASHEIGHTNAME       = 'Höhe';

procedure Register;

implementation

{%REGION TPictureDrawer}

procedure PictureDrawerMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas     : IPGL2DCanvas;
  ATop, ALeft: Double;
begin
  with TPictureDrawer(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;
    ATop:=-Canvas.Height/2.0;

    DrawPic(ALeft, ATop, -ALeft, -ATop);
  end;
end;

procedure PictureDrawerPathChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas     : IPGL2DCanvas;
  ATop, ALeft: Double;
begin
  with TPictureDrawer(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;
    ATop:=-Canvas.Height/2.0;

    LoadPic(IChangedString(SenderData).Value);
  end;
end;

procedure CreatePictureDrawer(APrototype: IPVisualisationPrototype); cdecl;
begin
  TPictureDrawer.Create(APrototype);
end;

constructor TPictureDrawer.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FTexID:=0;
  FCurrentPath:=EmptyString;

  FMain:=CallInputs[MAININPUTNAME];
  FPath:=StringInputs[PICTUREDRAWERPATHNAME];
  FPath.AddListener(@PictureDrawerPathChanged, Self, Environment.Thread);
  FMain.AddListener(@PictureDrawerMainCalled, Self, Environment.Thread);

  //to make shure the picture is loaded if the Path was not empty at creation time
  LoadPic(FPath.Get);
end;

destructor TPictureDrawer.Destroy;
begin
  FPath.RemoveListener(@PictureDrawerPathChanged, Self);
  FMain.RemoveListener(@PictureDrawerMainCalled, Self);
  FPath:=nil;
  FMain:=nil;
  inherited Destroy;
end;

procedure TPictureDrawer.DrawPic(X1, Y1, X2, Y2: GLFloat);
begin
  glBindTexture(GL_TEXTURE_2D, FTexID);
  glBegin(GL_QUADS);
          glColor4ub($FF,$FF,$FF,$FF);
          glTexCoord2f(0.0,0.0);
          glVertex2f(X1, Y1);
          glTexCoord2f(1.0,0.0);
          glVertex2f(X2, Y1);
          glTexCoord2f(1.0,1.0);
          glVertex2f(X2, Y2);
          glTexCoord2f(0.0,1.0);
          glVertex2f(X1, Y2);
  glEnd();
  glBindTexture(GL_TEXTURE_2D, 0);
end;

procedure TPictureDrawer.LoadPic(AFileName: string);
var
  AIntfImage: TLazIntfImage;
begin
  if (FCurrentPath <> AFileName) and FileExists(AFileName) then begin
    FCurrentPath:=AFileName;
    //load image
    AIntfImage:=TLazIntfImage.Create(0,0,[riqfRGB,riqfAlpha]);
    AIntfImage.LoadFromFile(AFileName);
    //update texture
    if FTexID=0 then glGenTextures(1, @FTexID);
    glBindTexture(GL_TEXTURE_2D, FTexID);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
    //glTexParameteri(GL_TEXTURE_2D,GL_GENERATE_MIPMAP,GL_TRUE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S,GL_CLAMP_TO_EDGE);
    glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T,GL_CLAMP_TO_EDGE);
    glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE);

    //glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB8, NextPowerOfTwo(Round(Width)), NextPowerOfTwo(Round(Height)),0, GL_RGB, GL_UNSIGNED_BYTE, ATexMem);
    gluBuild2DMipmaps(GL_TEXTURE_2D, 4, AIntfImage.Width, AIntfImage.Height, GL_BGRA, GL_UNSIGNED_BYTE, AIntfImage.GetDataLineStart(0));

    glBindTexture(GL_TEXTURE_2D, 0);

    AIntfImage.Destroy;
  end;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDPICTUREDRAWER, @CreatePictureDrawer);
    with CreatePreset('Picture Drawer', VIDPICTUREDRAWER) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Media');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, PICTUREDRAWERPATHNAME, '');
    end;
    //not implemented (because it is useless), but here are the specifications:
    {
    RegisterVis(VIDCANVAS, @CreateCanvas);
    with CreatePreset('Canvas', PIDCANVAS, VIDCANVAS) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Legacy');
      AddTag(TAGPREDEFINED);
      AddTag(TAGDEPRECATED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, CANVASWIDTHNAME, 640);
      AddInput(This, CANVASHEIGHTNAME, 480);
    end;
    }
  end;
  Application.Initialize;
end;

{%ENDREGION}

end.

