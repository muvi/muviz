unit StdVisualisations;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, VisEventImpl, SpectrumData, PresetType,
  StdParamTypes, ImportType, AdvGlFunc, StdTags, ValueStorages, MStrings,
  CanvasType, GUIDop, GraphX32, AdvCoord, SimpleVis, VisAddInput,
  VisualisationUtils;

type
  TLightning = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FColor: IPColor;
    FLimit: IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TWave      = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FColor: IPColor;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TBassRect  = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FColor: IPColor;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TClear     = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FColor: IPColor;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TFFTData   = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FColor: IPColor;
    FScale: IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TPeak      = class (TVisualisationEvents)
  private
    FMain : IPCall;
    FColor: IPColor;
    FScale: IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

procedure Register;

const
  VIDLIGHTNING : TGUID = '{2C75A06B-5A8D-471A-4C49-4748544E494E}';
  VIDWAVE      : TGUID = '{2C75A06B-5A8D-471A-5741-564556495320}';
  VIDBASSRECT  : TGUID = '{2C75A06B-5A8D-471A-4241-535352454354}';
  VIDCLEAR     : TGUID = '{2C75A06B-5A8D-471A-5F5F-5F5F5F5F5F5F}';
  VIDFFTDATA   : TGUID = '{2C75A06B-5A8D-471A-4646-542044415441}';
  VIDPEAK      : TGUID = '{2C75A06B-5A8D-471A-2020-5045414B2020}';

  VIDLIGHTNINGA: TGUID = '{2DBD9FEE-952A-4172-ABC0-BA74E7BBECB0}';

  {
  PIDLIGHTNING : TGUID = '{E6B2C54B-EF1D-4082-8793-0B94846EF937}';
  PIDLIGHTNINGA: TGUID = '{7C85E5E7-D8D2-43CF-8CB4-D46E8A589DC0}';
  PIDWAVE      : TGUID = '{66A12BC4-55E8-438A-8FB4-31C9C2E46707}';
  PIDBASSRECT  : TGUID = '{9F70CB54-036A-4AF9-BA7F-89E8C210A6B1}';
  PIDCLEAR     : TGUID = '{B39EC53E-D4C5-49D3-857E-AA91AE76C19E}';
  PIDFFTDATA   : TGUID = '{359A5585-456C-4B86-83FD-BAFB6BE97157}';
  PIDPEAK      : TGUID = '{B240753D-A931-49C4-BAA5-DF57F6D1E5E7}';
  }

  LIGHTNINGLIMITNAME  = 'Grenzwert';
  PEAKSCALENAME       = 'Streckfaktor';
  FFTDATASCALENAME    = 'Streckfaktor';

implementation

{%REGION imported stuff}

function Lighten(C: TVColor; Amount: Integer): TVColor;
var
  r, g, b, a: Integer;
begin
  a := C shr 24;
  r := (C and $00FF0000) shr 16;
  g := (C and $0000FF00) shr 8;
  b := C and $000000FF;

  Inc(r, Amount);
  Inc(g, Amount);
  Inc(b, Amount);

  if r > 255 then r := 255 else if r < 0 then r := 0;
  if g > 255 then g := 255 else if g < 0 then g := 0;
  if b > 255 then b := 255 else if b < 0 then b := 0;

  Result := a shl 24 + r shl 16 + g shl 8 + b;
end;

{%ENDREGION}
{%REGION TLightning}

threadvar
  rot1, rot2: Double;

procedure LightningMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Cx, Cy,
  Dx, Dy,
  Fx, Fy,
  d, w,
  x1, x2, y1, y2, vl,
  CosR1, SinR1,
  CosR2, SinR2: Double;
  i           : Integer;
  AColor      : TVColor;
const
  pi2: Double = 6.283; // 2*pi
begin
  with TLightning(Context) do begin
    //GetVisParams(Visualisation,AParams);
    AColor:=FColor.Value;
    if AdvSpectrumData.Peak[0]{LeftPeak}+AdvSpectrumData.Peak[1]{RightPeak} > FLimit.Value then
    begin
      //WaveData := Source.getwavedata;
      //Cx := Dest.Width/2;
      //Cy := Dest.Height/2;
      Cx:=0;
      Cy:=0;
      if random(10) = 0 then rot1 := random*pi2;
      if random(10) = 0 then rot2 := random*pi2;
      CosR1 := cos(rot1)*10;
      SinR1 := sin(rot1)*10;
      CosR2 := cos(rot2)*10;
      SinR2 := sin(rot2)*10;

      // Draw Channel 1
      Dx := Cx+CosR1;
      Dy := Cy+SinR1;
      x1 := Cx;
      y1 := Cy;
      i  := 0;
      repeat
        Fx := Dx-Cx;
        Fy := Dy-Cy;
        vl := sqrt(sqr(Fx)+sqr(Fy));
        d  := vl{ / 128};
        w  := (AdvSpectrumData.WaveData[i,0]{ / 128})*d;
        x2 := Dx+(Fy / vl)*w;
        y2 := Dy-(Fx / vl)*w;
        Line(x1, y1, x2, y2, Lighten(AColor, -i*3));
        x1 := x2;
        y1 := y2;
        Dx := Dx + CosR1;
        Dy := Dy + SinR1;
        inc(i, 4);
      until i >= 80;

      // Draw Channel 2
      Dx := Cx+CosR2;
      Dy := Cy+SinR2;
      x1 := Cx;
      y1 := Cy;
      i  := 1;
      repeat
        Fx := Dx-Cx;
        Fy := Dy-Cy;
        vl := sqrt(sqr(Fx)+sqr(Fy));
        d  := vl{ / 128};
        w  := (AdvSpectrumData.WaveData[i,0]{ / 128})*d;
        x2 := Dx+(Fy / vl)*w;
        y2 := Dy-(Fx / vl)*w;
        Line(x1, y1, x2, y2, Lighten(AColor, -i*3));
        x1 := x2;
        y1 := y2;
        Dx := Dx + CosR2;
        Dy := Dy + SinR2;
        inc(i, 4);
      until i >= 80;
    end;
  end;
end;

procedure CreateLightning(APrototype: IPVisualisationPrototype); cdecl;
begin
  TLightning.Create(APrototype);
end;

constructor TLightning.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@LightningMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C1NAME];
  FLimit:=FloatInputs[LIGHTNINGLIMITNAME];
end;

destructor TLightning.Destroy;
begin
  FMain.RemoveListener(@LightningMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  FLimit:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TWave}

procedure WaveMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I,YPos1,YPos2,r,g,b,c1                  : Integer;
  XSpace,YFac,AYOffset,BowFac, Atop, ALeft: Double;
  OldPos,NewPos                           : TRealPoint;
  Canvas                                  : IPGL2DCanvas;
  AColor                                  : TVColor;
begin
  with TWave(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ATop:=-Canvas.Height/2.0;
    ALeft:=-Canvas.Width/2.0;
    AColor:=FColor.Value;
    XSpace:=Canvas.Width/AdvSpectrumData.LongWaveDataCount;
    YPos1:=Round(Canvas.Height) div 6;
    YPos2:=Round(Canvas.Height)-YPos1;
    OldPos.Y:=ATop;
    OldPos.X:=ALeft;
    YFac:=Canvas.Height/6;
    BowFac:=pi/AdvSpectrumData.LongWaveDataCount;
    I:=0;
    repeat
      NewPos.X:=Round(I*XSpace) + ALeft;
      NewPos.Y:=Round(AdvSpectrumData.WaveDataLong[I,0]*YFac) + ATop;
      AYOffset:=sin(I*BowFac)*(Canvas.Height/6);

      c1 := abs(Round(AdvSpectrumData.WaveDataLong[I,0]*255{32767}){ div 128}); // same as l2, but absolute and *4

      r := RedComponent(AColor)   + c1;
      g := GreenComponent(AColor) + c1;
      b := BlueComponent(AColor)  + c1;
      if r > 255 then r := 255;
      if g > 255 then g := 255;
      if b > 255 then b := 255;

      Line(OldPos.X,OldPos.Y+YPos1+AYOffset,NewPos.X,NewPos.Y+YPos1+AYOffset,Color32(r,g,b));
      Line(OldPos.X,OldPos.Y+YPos2-AYOffset,NewPos.X,NewPos.Y+YPos2-AYOffset,Color32(r,g,b));
      OldPos:=NewPos;
      Inc(I,8);
    until I>=AdvSpectrumData.LongWaveDataCount;
  end;
end;

procedure CreateWave(APrototype: IPVisualisationPrototype); cdecl;
begin
  TWave.Create(APrototype);
end;

constructor TWave.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@WaveMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C1NAME];
end;

destructor TWave.Destroy;
begin
  FMain.RemoveListener(@WaveMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBassRect}

procedure BassRectMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  BassValue,I: Integer;
  BassValue2 : Double;
begin
  with TBassRect(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    BassValue2:=0;
    for I:=0 to 1 do BassValue2+=AdvSpectrumData.Levels[I,0];
    BassValue:=Trunc(BassValue2*200);
    FillRect(-BassValue,-BassValue,BassValue,BassValue,FColor);
  end;
end;

procedure CreateBassRect(APrototype: IPVisualisationPrototype); cdecl;
begin
  TBassRect.Create(APrototype);
end;

constructor TBassRect.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@BassRectMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C1NAME];
end;

destructor TBassRect.Destroy;
begin
  FMain.RemoveListener(@BassRectMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TClear}

procedure ClearMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TClear(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Clear(FColor);
  end;
end;

procedure CreateClear(APrototype: IPVisualisationPrototype); cdecl;
begin
  TClear.Create(APrototype);
end;

constructor TClear.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@ClearMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C3NAME];
end;

destructor TClear.Destroy;
begin
  FMain.RemoveListener(@ClearMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TFFTData}

procedure FFTDataMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  I                     : Integer;
  DataWidth, ALeft, ATop: Real;
  Canvas                : IPGL2DCanvas;
  AColor                : TVColor;
  AScale                : TVFloat;
begin
  with TFFTData(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;
    ATop:=-Canvas.Height/2.0;
    DataWidth:=Canvas.Width/AdvSpectrumData.FreqCount;
    AColor:=FColor.Value;
    AScale:=FScale.Value;
    for I:=0 to AdvSpectrumData.FreqCount-1
      do FillRect(I*DataWidth+ALeft,Canvas.Height+ATop-AdvSpectrumData.Levels[I,0]*Canvas.Height*AScale,(I+1)*DataWidth+ALeft,Canvas.Height+ATop,AColor);
  end;
end;

procedure CreateFFTData(APrototype: IPVisualisationPrototype); cdecl;
begin
  TFFTData.Create(APrototype);
end;

constructor TFFTData.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@FFTDataMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C1NAME];
  FScale:=FloatInputs[FFTDATASCALENAME];
end;

destructor TFFTData.Destroy;
begin
  FMain.RemoveListener(@FFTDataMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  FScale:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TPeak}

procedure PeakMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas     : IPGL2DCanvas;
  ALeft, ATop: Real;
begin
  with TPeak(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;
    ATop:=-Canvas.Height/2.0;
    FillRect(ALeft,ATop+Canvas.Height-AdvSpectrumData.Peak[0]*Canvas.Height*FScale.Get,ALeft+Canvas.Width,ATop+Canvas.Height,FColor);
  end;
end;

procedure CreatePeak(APrototype: IPVisualisationPrototype); cdecl;
begin
  TPeak.Create(APrototype);
end;

constructor TPeak.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@PeakMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C2NAME];
  FScale:=FloatInputs[PEAKSCALENAME];
end;

destructor TPeak.Destroy;
begin
  FMain.RemoveListener(@PeakMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  FScale:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  PresetUtil.RegisterVis(VIDLIGHTNING, @CreateLightning);
  with CreatePreset('Lightning', VIDLIGHTNING) do begin
    AddTag(TAGLISTED);
    AddTag('Layers.Simple');
    AddTag(TAGPREDEFINED);
    AddInput(This, MAININPUTNAME);
    AddInput(This, C1NAME, $FFFF0000);
    AddInput(This, LIGHTNINGLIMITNAME, 0.1);
  end;
  with CreatePreset('Lightning with autocall', VIDLIGHTNINGA) do begin
    AddTag(TAGLISTED);
    AddTag('Layers.Simple');
    AddTag(TAGPREDEFINED);
    //add system inputs
    AddInput(This, PARENTINPUTNAME, VIDLIGHTNING);
    //add other inputs
    AddInput(This, MAININPUTNAME);
    AddInput(This, C1NAME, $FFFF0000);
    AddInput(This, LIGHTNINGLIMITNAME, 0.1);
    //AddInput(This, AUTOCALLNAME, true);
  end;
  PresetUtil.RegisterVis(VIDWAVE, @CreateWave);
  with CreatePreset('Wave', VIDWAVE) do begin
    AddTag(TAGLISTED);
    AddTag('Layers.Simple');
    AddTag(TAGPREDEFINED);
    AddInput(This, MAININPUTNAME);
    AddInput(This, C1NAME, $FFFF0000);
    //AddInput(This, AUTOCALLNAME, true);
  end;
  PresetUtil.RegisterVis(VIDBASSRECT, @CreateBassRect);
  with CreatePreset('Bassrectangle', VIDBASSRECT) do begin
    AddTag(TAGLISTED);
    AddTag('Layers.Legacy');
    AddTag(TAGPREDEFINED);
    AddTag(TAGDEPRECATED);
    AddInput(This, MAININPUTNAME);
    AddInput(This, C1NAME, $FFFF0000);
    //AddInput(This, AUTOCALLNAME, true);
  end;
  PresetUtil.RegisterVis(VIDCLEAR, @CreateClear);
  with CreatePreset('Clear Layer', VIDCLEAR) do begin
    AddTag(TAGLISTED);
    AddTag('Layers.Simple Effects');
    AddTag(TAGPREDEFINED);
    AddInput(This, MAININPUTNAME);
    AddInput(This, C3NAME, $FF000000);
    //AddInput(This, AUTOCALLNAME, true);
  end;
  PresetUtil.RegisterVis(VIDFFTDATA, @CreateFFTData);
  with CreatePreset('FFT Data', VIDFFTDATA) do begin
    AddTag(TAGLISTED);
    AddTag('Layers.Simple');
    AddTag(TAGPREDEFINED);
    AddInput(This, MAININPUTNAME);
    AddInput(This, C1NAME, $FFFF0000);
    AddInput(This, FFTDATASCALENAME, 20.0);
    //AddInput(This, AUTOCALLNAME, true);
  end;
  PresetUtil.RegisterVis(VIDPEAK, @CreatePeak);
  with CreatePreset('Peak', VIDPEAK) do begin
    AddTag(TAGLISTED);
    AddTag('Layers.Simple');
    AddTag(TAGPREDEFINED);
    AddInput(This, MAININPUTNAME);
    AddInput(This, C2NAME, $FFFF0000);
    AddInput(This, PEAKSCALENAME, 20.0);
    //AddInput(This, AUTOCALLNAME, true);
  end;
end;

{%ENDREGION}

end.

