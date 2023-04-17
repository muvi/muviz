unit UltraVisUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags, GraphX32,
  AdvCoord, MPluginType4, SimpleVis, VisAddInput, VisualisationUtils;

type
  TPhiRArray = array of TPhiR;
  TVIntArray = array of Integer;

  TSpreadGobo = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FInnerColor1: IPColor; //C1
    FInnerColor2: IPColor; //C2
    FOuterColor1: IPColor; //C3
    FOuterColor2: IPColor; //C4
    FGoboSize   : IPInteger;
    FGoboCount  : IPInteger;
    FLimit      : IPFloat;
    FScale      : IPFloat;
    FRadius     : IPFloat;
    FSinkSpeed  : IPFloat;

    FGoboPos    : TPhiRArray;
    FOldGoboPos : TPhiRArray;
    FGoboLevels : TVIntArray;
    FGoboLevels2: TVIntArray;
    procedure GoboCountChanged(AGoboCount: TVInteger);
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TVox        = class (TVisualisationEvents)
  private
    FMain      : IPCall;
    FColor     : IPColor;
    FScale     : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TWaveCircle = class (TVisualisationEvents)
  private
    FMain          : IPCall;
    FInnerLineColor: IPColor;
    FOuterLineColor: IPColor;
    FRadius        : IPFloat;
    FIntensity     : IPFloat;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

const
  VIDSPREADGOBO   : TGUID = '{2C75A06B-5A8D-471A-5350-52454144474F}';
  VIDVOX          : TGUID = '{2C75A06B-5A8D-471A-564F-585649535F5F}';
  VIDWAVECIRCLE   : TGUID = '{2C75A06B-5A8D-471A-5741-564543495243}';

  {
  PIDSPREADGOBO   : TGUID = '{180D3FFB-E597-4580-B75C-C14467CE428E}';
  PIDVOX          : TGUID = '{F7F8F557-CF98-489F-B640-9C8789BE10E1}';
  PIDWAVECIRCLE   : TGUID = '{D789E706-30C0-46EE-933C-8BB703243561}';
  }

  SPREADGOBOINNERCOLOR1NAME    = C1NAME;
  SPREADGOBOINNERCOLOR2NAME    = C2NAME;
  SPREADGOBOOUTERCOLOR1NAME    = C3NAME;
  SPREADGOBOOUTERCOLOR2NAME    = '2. Gobofarbe Außen';
  SPREADGOBOGOBOSIZENAME       = 'Größe';
  SPREADGOBOGOBOCOUNTNAME      = 'Anzahl';
  SPREADGOBOLIMITNAME          = 'Grenzwert';
  SPREADGOBOSCALENAME          = 'Streckfaktor';
  SPREADGOBORADIUSNAME         = 'Maximalradius (in %)';
  SPREADGOBOSINKSPEEDNAME      = 'Absinkfaktor';
  VOXSCALENAME                 = 'Streckfaktor';
  WAVECIRCLEINNERLINECOLORNAME = C1NAME;
  WAVECIRCLEOUTERLINECOLORNAME = C2NAME;
  WAVECIRCLERADIUSNAME         = 'Radius (in %)';
  WAVECIRCLEINTENSITYNAME      = 'Ausprägung';

  PhiRSize                     = SizeOf(TPhiR);
  IntSize                      = SizeOf(Integer);

procedure Register;

implementation

{%REGION TSpreadGobo}

procedure SpreadGoboMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas                                                : IPGL2DCanvas;
  Bass, Mid, Treble                                     : Real;
  i                                                     : Integer;
  tr                                                    : Real;
  tx,ty                                                 : Real;
  AActualLimit                                          : Real;
  AC1,AC2                                               : TVColor;
  ARadius                                               : Real;

  AInnerColor1, AInnerColor2, AOuterColor1, AOuterColor2: TVColor;
  AScale, ASinkSpeed, ALimit                            : TVFloat;
  AGoboSize                                             : TVInteger;
begin
  with TSpreadGobo(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);

    Bass:=0.0;
    Mid:=0.0;
    Treble:=0.0;
    for i:=0 to 3
      do Bass += AdvSpectrumData.Levels[i,0];
    for i:=AdvSpectrumData.FreqCount div 8 to AdvSpectrumData.FreqCount div 4
      do Mid += AdvSpectrumData.Levels[i,0];
    for i:=AdvSpectrumData.FreqCount div 8 * 3 to AdvSpectrumData.FreqCount div 2
      do Treble+=AdvSpectrumData.Levels[i,0];
    if Canvas.Height>Canvas.Width
      then ARadius:=Canvas.Width*(FRadius.Get/200)
      else ARadius:=Canvas.Height*(FRadius.Get/200);

    AInnerColor1:=FInnerColor1.Value;
    AInnerColor2:=FInnerColor2.Value;
    AOuterColor1:=FOuterColor1.Value;
    AOuterColor2:=FOuterColor2.Value;
    AScale:=FScale.Value;
    ASinkSpeed:=FSinkSpeed.Value;
    AGoboSize:=FGoboSize.Value;
    ALimit:=FLimit.Value;

    for i:=0 to Length(FGoboPos)-1 do begin
      //Schredderwert
      AActualLimit:=FLimit.Get-(AdvSpectrumData.Levels[FGoboLevels2[i],0]*10);

      if AdvSpectrumData.Levels[FGoboLevels[i],0]>AActualLimit then begin
        tr:=((AdvSpectrumData.Levels[FGoboLevels[i],0]-AActualLimit)*AScale);
      end else begin
        tr:=-FGoboPos[i].R*(AActualLimit-AdvSpectrumData.Levels[FGoboLevels[i],0])*ASinkSpeed;
      end;

      with FGoboPos[i] do begin
        R+=tr;
        if R>ARadius then begin
          R:=frac(R/ARadius)*ARadius;
          Phi:=Random*2*Pi;
        end;
        tx:=R*Sin(Phi);
        ty:=R*Cos(Phi);
        AC1:=BetaBlend(AOuterColor1, AInnerColor1, Round(($FF/ARadius)*R));
        AC2:=BetaBlend(AOuterColor2, AInnerColor2, Round(($FF/ARadius)*R));
      end;
      FOldGoboPos[i].R:=FGoboPos[i].R;
      FOldGoboPos[i].Phi:=FGoboPos[i].Phi;
      FillRect(tx,ty,tx+AGoboSize,ty+AGoboSize,BetaBlend(AC1,AC2,Round(($FF/(AdvSpectrumData.FreqCount div 8))*FGoboLevels[i])));
    end;
  end;
end;

procedure SpreadGoboGoboCountChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TSpreadGobo(Context).GoboCountChanged(IChangedInteger(SenderData).Value);
end;

procedure CreateSpreadGobo(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSpreadGobo.Create(APrototype);
end;

constructor TSpreadGobo.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@SpreadGoboMainCalled, Self, Environment.Thread);
  FInnerColor1:=ColorInputs[SPREADGOBOINNERCOLOR1NAME];
  FInnerColor2:=ColorInputs[SPREADGOBOINNERCOLOR2NAME];
  FOuterColor1:=ColorInputs[SPREADGOBOOUTERCOLOR1NAME];
  FOuterColor2:=ColorInputs[SPREADGOBOOUTERCOLOR2NAME];
  FGoboSize:=IntegerInputs[SPREADGOBOGOBOSIZENAME];
  FGoboCount:=IntegerInputs[SPREADGOBOGOBOCOUNTNAME];
  FGoboCount.AddListener(@SpreadGoboGoboCountChanged, Self, Environment.Thread);
  FLimit:=FloatInputs[SPREADGOBOLIMITNAME];
  FScale:=FloatInputs[SPREADGOBOSCALENAME];
  FRadius:=FloatInputs[SPREADGOBORADIUSNAME];
  FSinkSpeed:=FloatInputs[SPREADGOBOSINKSPEEDNAME];
  //init
  GoboCountChanged(FGoboCount.Value);
end;

destructor TSpreadGobo.Destroy;
begin
  //free
  with PluginSystem do begin
    SetLength(FGoboPos, 0);
    SetLength(FOldGoboPos,0);
    SetLength(FGoboLevels,0);
    SetLength(FGoboLevels2,0);
  end;

  FMain.RemoveListener(@SpreadGoboMainCalled, Self);
  FMain:=nil;
  FInnerColor1:=nil;
  FInnerColor2:=nil;
  FOuterColor1:=nil;
  FOuterColor2:=nil;
  FGoboSize:=nil;
  FGoboCount.RemoveListener(@SpreadGoboGoboCountChanged, Self);
  FGoboCount:=nil;
  FLimit:=nil;
  FScale:=nil;
  FRadius:=nil;
  FSinkSpeed:=nil;
  inherited Destroy;
end;

procedure TSpreadGobo.GoboCountChanged(AGoboCount: TVInteger);
var
  I, FreqArea: Integer;
  PhiFac     : Real;
begin
  if AGoboCount < 0
    then AGoboCount:=0;
  SetLength(FGoboPos, AGoboCount);
  SetLength(FOldGoboPos, AGoboCount);
  SetLength(FGoboLevels, AGoboCount);
  SetLength(FGoboLevels2, AGoboCount);

  PhiFac:=(2*Pi)/AGoboCount;
  FreqArea:=512 div 8;
  for I:=0 to AGoboCount-1 do begin
    with FGoboPos[I] do begin
      Phi:=PhiFac*I;
      R:=0.0;
    end;
    with FOldGoboPos[I] do begin
      Phi:=FGoboPos[I].Phi;
      R:=0.0;
    end;
    FGoboLevels[I]:=random(FreqArea);
    FGoboLevels2[I]:=random(FreqArea)+(FreqArea*2);
  end;
end;

{%ENDREGION}
{%REGION TVox}

procedure VoxMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas                : IPGL2DCanvas;
  ALeft                 : Double;
  I,J,DC                : Integer;
  DataWidth, AFreqHeight: Real;

  AScale                : TVFloat;
  AColor                : TVColor;
begin
  with TVox(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    ALeft:=-Canvas.Width/2.0;

    DataWidth:=Canvas.Width/AdvSpectrumData.FreqCount;
    DC:=AdvSpectrumData.FreqCount div 2;
    J:=0;

    AScale:=FScale.Value;
    AColor:=FColor.Value;
    for I:=DC to AdvSpectrumData.FreqCount-1 do begin
      AFreqHeight:=Round(AdvSpectrumData.Levels[I,0]*Canvas.Height*AScale);
      FillRect(ALeft+J*DataWidth,-AFreqHeight,ALeft+(J+1)*DataWidth,AFreqHeight,AColor);
      Inc(J);
    end;
    for I:=0 to DC-1 do begin
      AFreqHeight:=Round(AdvSpectrumData.Levels[I,0]*Canvas.Height*AScale);
      FillRect(ALeft+J*DataWidth,-AFreqHeight,ALeft+(J+1)*DataWidth,AFreqHeight,AColor);
      Inc(J);
    end;
  end;
end;

procedure CreateVox(APrototype: IPVisualisationPrototype); cdecl;
begin
  TVox.Create(APrototype);
end;

constructor TVox.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@VoxMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[C1NAME];
  FScale:=FloatInputs[VOXSCALENAME];
end;

destructor TVox.Destroy;
begin
  FMain.RemoveListener(@VoxMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  FScale:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TWaveCircle}

procedure WaveCircleMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas                          : IPGL2DCanvas;
  APhiStep, ar, rr, AAdjust       : Real;
  I                               : Integer;
  LastPoint,NewPoint,FirstPoint   : TRealPoint;

  AInnerLineColor, AOuterLineColor: TVColor;
  AIntensity                      : TVFloat;

  function GetALineColor(const AValue: Real): TVColor;
  var
    c1: Integer;
  begin
    c1 := abs(Round(AValue*300));
    if c1>255 then c1:=255;
    c1:=255-c1;
    with TWaveCircle(Context)
      do Result:=BetaBlend(AInnerLineColor, AOuterLineColor, c1);
  end;

begin
  with TWaveCircle(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);

    AInnerLineColor:=FInnerLineColor.Value;
    AOuterLineCOlor:=FOuterLineColor.Value;
    AIntensity:=FIntensity.Value;

    APhiStep:=(2*Pi)/AdvSpectrumData.WaveDataCount;
    I:=1;
    if Canvas.Height<Canvas.Width
      then rr:=(FRadius.Get/200.0)*Canvas.Height
      else rr:=(FRadius.Get/200.0)*Canvas.Width;
    ar:=rr+(AIntensity*AdvSpectrumData.WaveData[0,0]);
    FirstPoint.X:=ar*sin(0);
    FirstPoint.Y:=ar*cos(0);
    LastPoint:=FirstPoint;
    //prevents the creation of an ugly line at the bottom of the circle
    with AdvSpectrumData
      do AAdjust:=(WaveData[0, 0]-WaveData[WaveDataCount-1, 0]) / WaveDataCount;
    while I<AdvSpectrumData.WaveDataCount do begin

      ar:=rr+(AIntensity*(AdvSpectrumData.WaveData[I,0]+AAdjust*I));
      NewPoint.X:=ar*sin(I*APhiStep);
      NewPoint.Y:=ar*cos(I*APhiStep);

      Line(LastPoint.X,LastPoint.Y,NewPoint.X,NewPoint.Y,GetALineColor(AdvSpectrumData.WaveData[I,0]));
      LastPoint:=NewPoint;
      Inc(I);
    end;
    Line(LastPoint.X,LastPoint.Y,FirstPoint.X,FirstPoint.Y,GetALineColor(AdvSpectrumData.WaveData[0,0]));
  end;
end;

procedure CreateWaveCircle(APrototype: IPVisualisationPrototype); cdecl;
begin
  TWaveCircle.Create(APrototype);
end;

constructor TWaveCircle.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@WaveCircleMainCalled, Self, Environment.Thread);
  FInnerLineColor:=ColorInputs[WAVECIRCLEINNERLINECOLORNAME];
  FOuterLineColor:=ColorInputs[WAVECIRCLEOUTERLINECOLORNAME];
  FRadius:=FloatInputs[WAVECIRCLERADIUSNAME];
  FIntensity:=FloatInputs[WAVECIRCLEINTENSITYNAME];
end;

destructor TWaveCircle.Destroy;
begin
  FMain.RemoveListener(@WaveCircleMainCalled, Self);
  FMain:=nil;
  FInnerLineColor:=nil;
  FOuterLineColor:=nil;
  FRadius:=nil;
  FIntensity:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDSPREADGOBO, @CreateSpreadGobo);
    with CreatePreset('Spread Gobo', VIDSPREADGOBO) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, SPREADGOBOINNERCOLOR1NAME, $FF00FF00);
      AddInput(This, SPREADGOBOINNERCOLOR2NAME, $FF00FF00);
      AddInput(This, SPREADGOBOOUTERCOLOR1NAME, TVColor($0000FF00));
      AddInput(This, SPREADGOBOOUTERCOLOR2NAME, TVColor($0000FF00));
      AddInput(This, SPREADGOBOGOBOSIZENAME, 3);
      AddInput(This, SPREADGOBOGOBOCOUNTNAME, 20);
      AddInput(This, SPREADGOBOLIMITNAME, 0.015);
      AddInput(This, SPREADGOBOSCALENAME, 1000.0);
      AddInput(This, SPREADGOBORADIUSNAME, 100.0);
      AddInput(This, SPREADGOBOSINKSPEEDNAME, 1.0);
    end;
    RegisterVis(VIDVOX, @CreateVox);
    with CreatePreset('Vox', VIDVOX) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, C1NAME, $FFFF0000);
      AddInput(This, VOXSCALENAME, 10.0);
    end;
    RegisterVis(VIDWAVECIRCLE, @CreateWaveCircle);
    with CreatePreset('Wave Circle', VIDWAVECIRCLE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, WAVECIRCLEINNERLINECOLORNAME, $FFFF0000);
      AddInput(This, WAVECIRCLEOUTERLINECOLORNAME, TVColor($7FFF0000));
      AddInput(This, WAVECIRCLERADIUSNAME, 70.0);
      AddInput(This, WAVECIRCLEINTENSITYNAME, 20.0);
    end;
  end;
  randomize;
end;

{%ENDREGION}

end.

