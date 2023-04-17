unit DiscoUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags, GraphX32,
  AdvCoord, Math, SimpleVis, VisAddInput, VisualisationUtils;

type
  TStrobe     = class (TVisualisationEvents)
  private
    FMain    : IPCall;
    FOnColor : IPColor;
    FOffColor: IPColor;
    FOnTime  : IPInteger;
    FOffTime : IPInteger;

    FPosition: Cardinal;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TRythmStrobe= class (TVisualisationEvents)
  private
    FMain    : IPCall;
    FOnColor : IPColor;
    FOffColor: IPColor;
    FLimit   : IPFloat;
    FScale   : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TLines      = class (TVisualisationEvents)
  private
    FMain      : IPCall;
    FColor     : IPColor;
    FLineLength: IPFloat;
    FDistance  : IPFloat;
    FLimit     : IPFloat;

    FPosition  : TPhiR;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TRythmCircle= class (TVisualisationEvents)
  private
    FMain    : IPCall;
    FColor   : IPColor;
    FR       : IPFloat;
    FSpeed   : IPFloat;

    FPhi     : Real;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

const
  VIDSTROBE       : TGUID = '{2C75A06B-5A8D-471A-2053-54524F424520}';
  VIDRYTHMSTROBE  : TGUID = '{2C75A06B-5A8D-471A-5253-54524F424520}';
  VIDLINES        : TGUID = '{2C75A06B-5A8D-471A-204C-494E45532020}';
  VIDRYTHMCIRCLE  : TGUID = '{2C75A06B-5A8D-471A-4352-434952434C45}';

  {
  PIDSTROBE       : TGUID = '{B6E3D247-54E0-4D6C-B26D-BB5F0CC347B1}';
  PIDRYTHMSTROBE  : TGUID = '{4DFAAF42-73E6-4AAF-85A4-359FC04D60DE}';
  PIDLINES        : TGUID = '{B54C3A2F-A079-41F5-BBFC-24F57B8ACA7C}';
  PIDRYTHMCIRCLE  : TGUID = '{ABF28440-FBFA-4565-8721-5131493D4ED8}';
  }

  STROBEONTIMENAME       = 'Blinkzeit';
  STROBEOFFTIMENAME      = 'Dunkelzeit';
  STROBEONCOLORNAME      = C1NAME;
  STROBEOFFCOLORNAME     = C2NAME;
  RYTHMSTROBELIMITNAME   = 'Grenzwert';
  RYTHMSTROBESCALENAME   = 'Streckfaktor';
  RYTHMSTROBEONCOLORNAME = C1NAME;
  RYTHMSTROBEOFFCOLORNAME= C2NAME;
  LINESLINELENGTHNAME    = 'Linienlänge';
  LINESDISTANCENAME      = 'Abstand zum Mittelpunkt';
  LINESLIMITNAME         = 'Grenzwert';
  RYTHMCIRCLERNAME       = 'Radius';
  RYTHMCIRCLESPEEDNAME   = 'Geschwindigkeit';

procedure Register;

implementation

{%REGION TStrobe}

procedure StrobeMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas           : IPGL2DCanvas;
  ATop, ALeft      : Double;
  AOnTime, AOffTime: TVInteger;
begin
  with TStrobe(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;
    ATop:=-Canvas.Height/2.0;

    if FPosition<FOnTime.Get
      then FillRect(ALeft, ATop, ALeft+Canvas.Width, ATop+Canvas.Height,FOnColor)
      else FillRect(ALeft, ATop, ALeft+Canvas.Width, ATop+Canvas.Height,FOffColor);
    Inc(FPosition);
    AOnTime:=FOnTime.Get;
    AOffTime:=FOffTime.Get;
    if AOnTime+AOffTime>0
      then FPosition:=FPosition mod (AOnTime+AOffTime)
      else FPosition:=0;
  end;
end;

procedure CreateStrobe(APrototype: IPVisualisationPrototype); cdecl;
begin
  TStrobe.Create(APrototype);
end;

constructor TStrobe.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@StrobeMainCalled, Self, Environment.Thread);
  FOnColor:=ColorInputs[STROBEONCOLORNAME];
  FOffColor:=ColorInputs[STROBEOFFCOLORNAME];
  FOnTime:=IntegerInputs[STROBEONTIMENAME];
  FOffTime:=IntegerInputs[STROBEOFFTIMENAME];

  FPosition:=0;
end;

destructor TStrobe.Destroy;
begin
  FMain.RemoveListener(@StrobeMainCalled, Self);
  FMain:=nil;
  FOnColor:=nil;
  FOffColor:=nil;
  FOnTime:=nil;
  FOffTime:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TRythmStrobe}

procedure RythmStrobeMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas     : IPGL2DCanvas;
  ATop, ALeft: Double;
  ABeta      : Int64;
begin
  with TRythmStrobe(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;
    ATop:=-Canvas.Height/2.0;

    //and $FF prevents errors with range checking
    ABeta:=Round((AdvSpectrumData.Peak[0]-FLimit.Get)*FScale.Get*$FF) and $FF;
    FillRect(ALeft, ATop, ALeft+Canvas.Width, ATop+Canvas.Height, BetaBlend(FOnColor.Get, FOffColor.Get, ABeta));
  end;
end;

procedure CreateRythmStrobe(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRythmStrobe.Create(APrototype);
end;

constructor TRythmStrobe.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@RythmStrobeMainCalled, Self, Environment.Thread);
  FOnColor:=ColorInputs[RYTHMSTROBEONCOLORNAME];
  FOffColor:=ColorInputs[RYTHMSTROBEOFFCOLORNAME];
  FLimit:=FloatInputs[RYTHMSTROBELIMITNAME];
  FScale:=FloatInputs[RYTHMSTROBESCALENAME];
end;

destructor TRythmStrobe.Destroy;
begin
  FMain.RemoveListener(@RythmStrobeMainCalled, Self);
  FMain:=nil;
  FOnColor:=nil;
  FOffColor:=nil;
  FLimit:=nil;
  FScale:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TLines}

procedure LinesMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  NewPos2    : TRealPoint;
  OldPos2    : TRealPoint;
  OldR       : Real;
begin
  with TLines(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    //init
    if AdvSpectrumData.Levels[0,0]<FLimit.Get then exit;
    OldPos2:=RealPoint(FPosition,ZEROCENTER);
    //draw
    with FPosition do begin
      OldR:=R;
      R:=AdvSpectrumData.WaveData[0,0]*FDistance.Get+1;
      Phi:=Phi+RToPhi(R,OldR,(AdvSpectrumData.Levels[0,0]+AdvSpectrumData.Levels[1,0]+AdvSpectrumData.Levels[2,0]+AdvSpectrumData.Levels[3,0])*FLineLength.Get);
    end;
    //done
    NewPos2:=RealPoint(FPosition,ZEROCENTER);
    Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,FColor);
  end;
end;

procedure CreateLines(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLines.Create(APrototype);
end;

constructor TLines.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@LinesMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C1NAME];
  FLineLength:=FloatInputs[LINESLINELENGTHNAME];
  FDistance:=FloatInputs[LINESDISTANCENAME];
  FLimit:=FloatInputs[LINESLIMITNAME];

  with FPosition do begin
    R:=100.0;
    Phi:=0.0;
  end;
end;

destructor TLines.Destroy;
begin
  FMain.RemoveListener(@LinesMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  FLineLength:=nil;
  FDistance:=nil;
  FLimit:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TRythmCircle}

procedure RythmCircleMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  NewPos2    : TRealPoint;
  OldPos2    : TRealPoint;
  AR         : TVFloat;
begin
  with TRythmCircle(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    AR:=FR.Get;
    //you wouldn't see something anyway, even if it would not crash
    if isZero(AR)
      then exit;
    //init
    OldPos2:=RealPoint(PhiR(AR,FPhi),ZEROCENTER);
    //draw
    FPhi:=FPhi+RToPhi(AR,AR,(AdvSpectrumData.Levels[0,0]+AdvSpectrumData.Levels[1,0]+AdvSpectrumData.Levels[2,0]+AdvSpectrumData.Levels[3,0])*FSpeed.Get);
    //done
    NewPos2:=RealPoint(PhiR(AR, FPhi),ZEROCENTER);
    Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,FColor);
  end;
end;

procedure CreateRythmCircle(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRythmCircle.Create(APrototype);
end;

constructor TRythmCircle.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@RythmCircleMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C1NAME];
  FR:=FloatInputs[RYTHMCIRCLERNAME];
  FSpeed:=FloatInputs[RYTHMCIRCLESPEEDNAME];

  FPhi:=0.0;
end;

destructor TRythmCircle.Destroy;
begin
  FMain.RemoveListener(@RythmCircleMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  FR:=nil;
  FSpeed:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDSTROBE, @CreateStrobe);
    with CreatePreset('Strobe', VIDSTROBE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, STROBEONCOLORNAME, $FFFFFFFF);
      AddInput(This, STROBEOFFCOLORNAME, $FF000000);
      AddInput(This, STROBEONTIMENAME, 2);
      AddInput(This, STROBEOFFTIMENAME, 2);
      //AddInput(This, AUTOCALLNAME, true);
    end;
    RegisterVis(VIDRYTHMSTROBE, @CreateRythmStrobe);
    with CreatePreset('Rythm Strobe', VIDRYTHMSTROBE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, RYTHMSTROBEONCOLORNAME, $FFFFFFFF);
      AddInput(This, RYTHMSTROBEOFFCOLORNAME, $FF000000);
      AddInput(This, RYTHMSTROBELIMITNAME, 2.0);
      AddInput(This, RYTHMSTROBESCALENAME, 20.0);
      //AddInput(This, AUTOCALLNAME, true);
    end;
    RegisterVis(VIDLINES, @CreateLines);
    with CreatePreset('Lines', VIDLINES) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, C1NAME, $FF0000FF);
      AddInput(This, LINESLINELENGTHNAME, 30000.0);
      AddInput(This, LINESDISTANCENAME, 1000.0);
      AddInput(This, LINESLIMITNAME, 0.0005);
      //AddInput(This, AUTOCALLNAME, true);
    end;
    RegisterVis(VIDRYTHMCIRCLE, @CreateRythmCircle);
    with CreatePreset('Rythm Circle', VIDRYTHMCIRCLE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, C1NAME, $FFFF0000);
      AddInput(This, RYTHMCIRCLERNAME, 100.0);
      AddInput(This, RYTHMCIRCLESPEEDNAME, 30000.0);
      //AddInput(This, AUTOCALLNAME, true);
    end;
  end;
end;

{%ENDREGION}

end.

