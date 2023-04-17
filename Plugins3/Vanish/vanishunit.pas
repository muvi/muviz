unit VanishUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags, VisualisationUtils,
  SimpleVis, VisAddInput;

type
  TVanish = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FColor: IPColor;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TRotate = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FSpeed: IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TZoom   = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FX    : IPFloat;
    FY    : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TMove   = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FX    : IPInteger;
    FY    : IPInteger;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

const
  VIDVANISH    : TGUID = '{2C75A06B-5A8D-471A-5F5F-56414E495348}';
  VIDROTATE    : TGUID = '{2C75A06B-5A8D-471A-5F5F-524F54415445}';
  VIDZOOM      : TGUID = '{2C75A06B-5A8D-471A-5F5F-5F5F5A4F4F4D}';
  VIDMOVE      : TGUID = '{2C75A06B-5A8D-471A-5F5F-5F5F4D4F5645}';
  VIDCRAZYSCALE: TGUID = '{2C75A06B-5A8D-471A-4352-415A59534341}';

  {
  PIDVANISH    : TGUID = '{B1B8F9FE-5A4A-4966-BAC3-E39DEE985E6E}';
  PIDROTATE    : TGUID = '{72DA44C8-F63D-4AD7-9F96-324B5E41E951}';
  PIDZOOM      : TGUID = '{BAD741FA-B5EC-4A3B-8D4A-08400EDCCDE8}';
  PIDMOVE      : TGUID = '{32BA8FCC-6224-4345-B348-280C30CA33C6}';
  }

  ROTATESPEEDNAME  = 'Geschwindigkeit';
  XNAME            = 'X';
  YNAME            = 'Y';

procedure Register;

implementation

{%REGION TVanish}

procedure VanishMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas     : IPGL2DCanvas;
  ATop, ALeft: Double;
begin
  with TVanish(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;
    ATop:=-Canvas.Height/2.0;
    FillRect(ALeft, ATop, ALeft+Canvas.Width, ATop+Canvas.Height, FColor);
  end;
end;

procedure CreateVanish(APrototype: IPVisualisationPrototype); cdecl;
begin
  TVanish.Create(APrototype);
end;

constructor TVanish.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@VanishMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C3NAME];
end;

destructor TVanish.Destroy;
begin
  FMain.RemoveListener(@VanishMainCalled, Self);
  FColor:=nil;
  FMain:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TRotate}

procedure RotateMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas: IPGL2DCanvas;
begin
  with TRotate(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    Rotate2D(Canvas.LastScreenTransformationMatrix^, FSpeed);
  end;
end;

procedure CreateRotate(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRotate.Create(APrototype);
end;

constructor TRotate.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@RotateMainCalled, Self, Environment.Thread);
  FSpeed:=FloatInputs[ROTATESPEEDNAME];
end;

destructor TRotate.Destroy;
begin
  FMain.RemoveListener(@RotateMainCalled, Self);
  FSpeed:=nil;
  FMain:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TZoom}

procedure ZoomMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas: IPGL2DCanvas;
begin
  with TZoom(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    Zoom2D(Canvas.LastScreenTransformationMatrix^, FX, FY);
  end;
end;

procedure CreateZoom(APrototype: IPVisualisationPrototype); cdecl;
begin
  TZoom.Create(APrototype);
end;

constructor TZoom.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@ZoomMainCalled, Self, Environment.Thread);
  FX:=FloatInputs[XNAME];
  FY:=FloatInputs[YNAME];
end;

destructor TZoom.Destroy;
begin
  FMain.RemoveListener(@ZoomMainCalled, Self);
  FX:=nil;
  FY:=nil;
  FMain:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TMove}

procedure MoveMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas: IPGL2DCanvas;
begin
  with TMove(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    Move2D(Canvas.LastScreenTransformationMatrix^, FX.Get, FY.Get);
  end;
end;

procedure CreateMove(APrototype: IPVisualisationPrototype); cdecl;
begin
  TMove.Create(APrototype);
end;

constructor TMove.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@MoveMainCalled, Self, Environment.Thread);
  FX:=IntegerInputs[XNAME];
  FY:=IntegerInputs[YNAME];
end;

destructor TMove.Destroy;
begin
  FMain.RemoveListener(@MoveMainCalled, Self);
  FX:=nil;
  FY:=nil;
  FMain:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDVANISH, @CreateVanish);
    with CreatePreset('Vanish', VIDVANISH) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple Effects');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, C3NAME, $FF000000);
    end;
    RegisterVis(VIDROTATE, @CreateRotate);
    with CreatePreset('Rotate', VIDROTATE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple Effects');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, ROTATESPEEDNAME, 2.0);
    end;
    RegisterVis(VIDMOVE, @CreateMove);
    with CreatePreset('Move', VIDMOVE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple Effects');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, XNAME, 0);
      AddInput(This, YNAME, 2);
    end;
    RegisterVis(VIDZOOM, @CreateZoom);
    with CreatePreset('Zoom', VIDZOOM) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple Effects');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, XNAME, 2.0);
      AddInput(This, YNAME, 2.0);
    end;
    //just for compatibility, so do not create a preset here.
    RegisterVis(VIDCRAZYSCALE, @CreateZoom);
  end;
end;

{%ENDREGION}

end.

