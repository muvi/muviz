unit FabiansVisUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags, AdvCoord, Math,
  GraphX32, MPluginType4, SimpleVis, ParamOp, VisAddInput, VisualisationUtils;

type
  TPhiRArray = array of TPhiR;
  TVIntArray = array of Integer;

  TGobo       = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FBassColor  : IPColor;
    FTrebleColor: IPColor;
    FGoboSize   : IPInteger;
    FGoboCount  : IPInteger;
    FLimit      : IPFloat;
    FScale      : IPFloat;

    FGoboPos    : TPhiRArray;
    FOldGoboPos : TPhiRArray;
    FGoboLevels : TVIntArray;
    FGoboLevels2: TVIntArray;
    procedure DoGoboCountChanged(AGoboCount: TVInteger);
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

const
  VIDGOBO     : TGUID = '{2C75A06B-5A8D-471A-476F-626F56497344}';

  GOBOBASSCOLORNAME   = C1NAME;
  GOBOTREBLECOLORNAME = C2NAME;
  GOBOSIZENAME        = 'GoboGröße';
  GOBOCOUNTNAME       = 'GoboAnzahl';
  GOBOLIMITNAME       = 'Grenzwert';
  GOBOSCALENAME       = 'Streckfaktor';

  PhiRSize            = SizeOf(TPhiR);
  IntSize             = SizeOf(Integer);
procedure Register;

implementation

{%REGION TGobo}

procedure GoboMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas                  : IPGL2DCanvas;

  i                       : Integer;
  tphi, tr, tx, ty        : Real;
  AActualLimit            : Real;

  ABassColor, ATrebleColor: TVColor;
  ALimit, AScale          : TVFloat;
  AGoboSize               : TVInteger;
begin
  with TGobo(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);

    ABassColor:=FBassColor.Value;
    ATrebleColor:=FTrebleColor.Value;
    ALimit:=FLimit.Value;
    AScale:=FScale.Value;
    AGoboSize:=FGoboSize.Value;

    Assert(not isNan(ALimit));
    Assert(not isNan(AScale));

    if AdvSpectrumData.Levels[Round((AdvSpectrumData.FreqCount/16)*3),0]>AdvSpectrumData.Levels[Round((AdvSpectrumData.FreqCount/16)*3)+1,0] then begin
      tphi:=AdvSpectrumData.Levels[Round((AdvSpectrumData.FreqCount/16)*3),0]*10;
    end else begin
      tphi:=-AdvSpectrumData.Levels[Round((AdvSpectrumData.FreqCount/16)*3)+1,0]*10;
    end;
    for i:=0 to Length(FGoboPos)-1 do begin
      //Schredderwert
      AActualLimit:=ALimit-(AdvSpectrumData.Levels[FGoboLevels2[i]{+(AdvSpectrumData.FreqCount div 8)},0]*10);

      if AdvSpectrumData.Levels[FGoboLevels[i],0]{Bass}>AActualLimit then begin
        tr:=((AdvSpectrumData.Levels[FGoboLevels[i],0]{Bass}-AActualLimit)*AScale);
      end else begin
        tr:=-FGoboPos[i].R*(AActualLimit-AdvSpectrumData.Levels[FGoboLevels[i],0]{Bass});
      end;

      with FGoboPos[i] do begin
        R+=tr;
        Phi+=tphi;
        tx:=R*Sin(Phi);
        ty:=R*Cos(Phi);
      end;
      FOldGoboPos[i].R:=FGoboPos[i].R;
      FOldGoboPos[i].Phi:=FGoboPos[i].Phi;
      FillRect(tx,ty,tx+AGoboSize,ty+AGoboSize,BetaBlend(ABassColor, ATrebleColor, Round(($FF/(AdvSpectrumData.FreqCount div 8))*FGoboLevels[i])));
    end;
  end;
end;

procedure GoboCountChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TGobo(Context).DoGoboCountChanged(IChangedInteger(SenderData).Value);
end;

procedure CreateGobo(APrototype: IPVisualisationPrototype); cdecl;
begin
  TGobo.Create(APrototype);
end;

procedure TGobo.DoGoboCountChanged(AGoboCount: TVInteger);
var
  I       : Integer;
  PhiFac  : Real;
  FreqArea: Integer;
begin
  if AGoboCount < 0
    then AGoboCount:=0;
  SetLength(FGoboPos, AGoboCount);
  SetLength(FOldGoboPos, AGoboCount);
  SetLength(FGoboLevels, AGoboCount);
  SetLength(FGoboLevels2, AGoboCount);

  if AGoboCount>0
    then PhiFac:=(2*Pi)/AGoboCount
    else exit;
  FreqArea:=AdvSpectrumData.FreqCount div 8;
  for I:=0 to AGoboCount-1 do begin
    Assert(I < AGoboCount);

    with FGoboPos[I] do begin
      Phi:=PhiFac*I;
      R:=0.0;
    end;

    //PiCut(GoboPos^[I].Phi);
    with FOldGoboPos[I] do begin
      Phi:=FGoboPos[I].Phi;
      R:=0.0;
    end;

    FGoboLevels[I]:=random(FreqArea);
    FGoboLevels2[I]:=random(FreqArea)+(FreqArea*2);
  end;
end;

constructor TGobo.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@GoboMainCalled, Self, Environment.Thread);
  FBassColor:=ColorInputs[GOBOBASSCOLORNAME];
  FTrebleColor:=ColorInputs[GOBOTREBLECOLORNAME];
  FGoboSize:=IntegerInputs[GOBOSIZENAME];
  FGoboCount:=IntegerInputs[GOBOCOUNTNAME];
  FGoboCount.AddListener(@GoboCountChanged, Self, Environment.Thread);
  FLimit:=FloatInputs[GOBOLIMITNAME];
  FScale:=FloatInputs[GOBOSCALENAME];

  //randomize;
  DoGoboCountChanged(FGoboCount.Value);
end;

destructor TGobo.Destroy;
begin
  FMain.RemoveListener(@GoboMainCalled, Self);
  FMain:=nil;
  FBassColor:=nil;
  FTrebleColor:=nil;
  FGoboSize:=nil;
  FGoboCount.RemoveListener(@GoboCountChanged, Self);
  FGoboCount:=nil;
  FLimit:=nil;
  FScale:=nil;

  with PluginSystem do begin
    SetLength(FGoboPos, 0);
    SetLength(FOldGoboPos, 0);
    SetLength(FGoboLevels, 0);
    SetLength(FGoboLevels2, 0);
  end;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDGOBO, @CreateGobo);
    with CreatePreset('Gobo', VIDGOBO) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, GOBOBASSCOLORNAME, $FFFF0000);
      AddInput(This, GOBOTREBLECOLORNAME, $FF00FF00);
      AddInput(This, GOBOSIZENAME, 20);
      AddInput(This, GOBOCOUNTNAME, 5);
      AddInput(This, GOBOLIMITNAME, 0.015);
      AddInput(This, GOBOSCALENAME, 1000.0);
    end;
  end;
end;

{%ENDREGION}

end.

