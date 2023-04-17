unit GeometryUnit;

{$mode objfpc}{$H+}

interface

uses
  SpectrumData, VisEventImpl, StdParamTypes, VisType2, CanvasType, GUIDop,
  MStrings, PresetType, ImportType, AdvGLFunc, StdTags, GraphX32,
  AdvCoord, Math, AdvFunc, SimpleVis, VisAddInput, VisualisationUtils;

type
  TNAngle               = class (TVisualisationEvents)
  private
    FMain       : IPCall;
    FColor      : IPColor;
    FRadius     : IPFloat;
    FCornerCount: IPInteger;
    FPositionX  : IPFloat;
    FPositionY  : IPFloat;
    FPhi        : IPFloat;
    FVisible    : IPBoolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TTriggerCircle        = class (TVisualisationEvents)
  private
    FMain      : IPCall;
    FInnerColor: IPColor;
    FOuterColor: IPColor;
    FRadius    : IPFloat;
    FPositionX : IPFloat;
    FPositionY : IPFloat;
    FPhi       : IPFloat;
    FScale     : IPFloat;
    FData      : IPBuffer;
    FVisible   : IPBoolean;

    FDrawSize  : Real;
  protected
    procedure InitMain; virtual;
    function DoGetBufferItem(AIndex: Integer): Real; virtual;
    property DrawSize: Real read FDrawSize write FDrawSize;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TSmoothedTriggerCircle= class (TTriggerCircle)
  private
    FAdjustBuf: Real;
  protected
    procedure InitMain; override;
    function DoGetBufferItem(AIndex: Integer): Real; override;
  end;

  TRain                 = class (TVisualisationEvents)
  private
    FMain      : IPCall;
    FColor1    : IPColor;
    FColor2    : IPColor;
    FRainLength: IPFloat;
    FRainDist  : IPFloat;
    FSpeed     : IPFloat;
    FPhi       : IPFloat;
    FScale     : IPFloat;
    FData      : IPBuffer;
    FVisible   : IPBoolean;

    FRainPos   : Real;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TTriggerParticles     = class (TVisualisationEvents)
  private
    FMain            : IPCall;
    FLeftTopColor    : IPColor;
    FRightTopColor   : IPColor;
    FLeftBottomColor : IPColor;
    FRightBottomColor: IPColor;
    FXData           : IPBuffer;
    FYData           : IPBuffer;
    FXScale          : IPFloat;
    FYScale          : IPFloat;
    FXOffset         : IPFloat;
    FYOffset         : IPFloat;
    FSize            : IPFloat;
    FVisible         : IPBoolean;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TProLightning         = class (TVisualisationEvents)
  private
    FMain        : IPCall;
    FLineColor   : IPColor;
    FBGColor     : IPColor;
    FRotation    : IPFloat;
    FAutoRotation: IPFloat;
    FWidth       : IPFloat;

    FRot         : Real;
    FR           : Real;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

  TSmoothyLine          = class (TVisualisationEvents)
  private
    FMain        : IPCall;
    FColor       : IPColor;
    FPosition    : IPFloat;
    FWidth       : IPFloat;
    FVertical    : IPBoolean;

    FScale       : Real;
    FLineOffset  : Real;
    FDrawProc    : procedure (APos: Real) of object;
    procedure DoOrientationChanged(AVertical: TVBoolean);
    procedure DoWidthChanged(AWidth: TVFloat);
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
    procedure DrawSmoothyLine_Vertical(APos: Real);
    procedure DrawSmoothyLine_Horizontal(APos: Real);
  end;

  TSmileyCircleDraw  = procedure (Pos: TRealPoint; R,Scale: Real; Color: TVColor; Buf: TVBuffer) of object;

  TSmiley               = class (TVisualisationEvents)
  private
    FMain            : IPCall;
    FSize            : IPFloat;
    FLeftEyeSize     : IPFloat;
    FRightEyeSize    : IPFloat;
    FMouthAngle      : IPFloat;
    FMouthOpen       : IPFloat;
    FColor           : IPColor;
    FLeftEyeColor    : IPColor;
    FRightEyeColor   : IPColor;
    FMouthColor      : IPColor;
    FData            : IPBuffer;
    FScale           : IPFloat;
    FLeftEyeData     : IPBuffer;
    FLeftEyeScale    : IPFloat;
    FRightEyeData    : IPBuffer;
    FRightEyeScale   : IPFloat;
    FMouthData       : IPBuffer;
    FMouthScale      : IPFloat;
    FMouthOpenData   : IPBuffer;
    FMouthOpenScale  : IPFloat;

    FLeftEyePos      : TRealPoint;
    FRightEyePos     : TRealPoint;
    FBodyRel         : Real;
    FEyeRel          : Real;
    FBodySize        : Real;
    FBodyScale       : Real;
    FDrawBody        : TSmileyCircleDraw;
    FDrawLeftEye     : TSmileyCircleDraw;
    FDrawRightEye    : TSmileyCircleDraw;

    DrawCircle       : array [Boolean] of TSmileyCircleDraw;
    procedure DoBodySizeChanged(ASize: TVFloat);
    procedure DrawDefaultCircle(Pos: TRealPoint; R,Scale: Real; Color: TVColor; Buf: TVBuffer);
    procedure DrawDataCircle(Pos: TRealPoint; R,Scale: Real; Color: TVColor; Buf: TVBuffer);
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
  end;

const
  VIDNANGLE               : TGUID = '{2C75A06B-5A8D-471A-6E41-4E474C455F5F}';
  VIDTRIGGERCIRCLE        : TGUID = '{2C75A06B-5A8D-471A-4349-52434C455F5F}';
  VIDSMOOTHEDTRIGGERCIRCLE: TGUID = '{2C75A06B-5A8D-471A-537E-434952434C45}';
  VIDRAIN                 : TGUID = '{2C75A06B-5A8D-471A-5F5F-5241494E5F5F}';
  VIDTRIGGERPARTICLES     : TGUID = '{2C75A06B-5A8D-471A-5472-696750617274}';
  VIDPROLIGHTNING         : TGUID = '{2C75A06B-5A8D-471A-5072-6F4C6774686E}';
  VIDSMOOTHYLINE          : TGUID = '{2C75A06B-5A8D-471A-536D-6F6F74684C6E}';
  VIDSMILEY               : TGUID = '{2C75A06B-5A8D-471A-536D-696C65793A29}';

  {
  PIDNANGLE               : TGUID = '{DF4FCCDC-F569-4A07-B451-ECD5991C7FDC}';
  PIDTRIGGERCIRCLE        : TGUID = '{887EDD78-77AC-4228-BFC0-21F38F81F5DE}';
  PIDSMOOTHEDTRIGGERCIRCLE: TGUID = '{576B2D24-0E41-45B1-90CA-CD410F2070E6}';
  PIDRAIN                 : TGUID = '{FB8D6129-6C36-4BD3-88DC-D1734947E994}';
  PIDTRIGGERPARTICLES     : TGUID = '{C1E2C6F5-0644-451E-830A-540262FBD5B3}';
  PIDPROLIGHTNING         : TGUID = '{593C929E-BD1C-4772-A9C9-D9FCCE3C001E}';
  PIDSMOOTHYLINE          : TGUID = '{F09E74C3-29E5-4B6F-8CB8-B427B0B9AAD8}';
  PIDSMILEY               : TGUID = '{784BFD99-59E1-4DB5-9D78-02BC93D27FB3}';
  }

  NANGLECOLORNAME                    = C1NAME;
  NANGLERADIUSNAME                   = 'Radius';
  NANGLECORNERCOUNTNAME              = 'Eckenanzahl';
  NANGLEXNAME                        = 'X-Position';
  NANGLEYNAME                        = 'Y-Position';
  NANGLEPHINAME                      = 'Drehung';
  NANGLEVISIBILITYNAME               = 'Sichtbar';
  TRIGGERCIRCLEINNERCOLORNAME        = C1NAME;
  TRIGGERCIRCLEOUTERCOLORNAME        = C2NAME;
  TRIGGERCIRCLERADIUSNAME            = 'Mindestradius';
  TRIGGERCIRCLEXNAME                 = 'X-Position';
  TRIGGERCIRCLEYNAME                 = 'Y-Position';
  TRIGGERCIRCLEPHINAME               = 'Drehung';
  TRIGGERCIRCLESCALENAME             = 'Streckfaktor';
  TRIGGERCIRCLEDATANAME              = 'Daten';
  TRIGGERCIRCLEVISIBILITYNAME        = 'Sichtbar';
  RAINCOLOR1NAME                     = C1NAME;
  RAINCOLOR2NAME                     = C2NAME;
  RAINLENGTHNAME                     = 'Regenlänge';
  RAINDISTNAME                       = 'Regenabstand';
  RAINSPEEDNAME                      = 'Geschwindikeit';
  RAINPHINAME                        = 'Drehung';
  RAINSCALENAME                      = 'Streckfaktor';
  RAINDATANAME                       = 'Daten';
  RAINVISIBILITYNAME                 = 'Sichtbar';
  TRIGGERPARTICLELEFTTOPCOLORNAME    = C1NAME;
  TRIGGERPARTICLERIGHTTOPCOLORNAME   = C2NAME;
  TRIGGERPARTICLELEFTBOTTOMCOLORNAME = C3NAME;
  TRIGGERPARTICLERIGHTBOTTOMCOLORNAME= 'Farbe Unten Rechts';
  TRIGGERPARTICLEDATAXNAME           = 'X-Werte';
  TRIGGERPARTICLEDATAYNAME           = 'Y-Werte';
  TRIGGERPARTICLESCALEXNAME          = 'X-Streckfaktor';
  TRIGGERPARTICLESCALEYNAME          = 'Y-Streckfaktor';
  TRIGGERPARTICLEOFFSETXNAME         = 'X-Offset';
  TRIGGERPARTICLEOFFSETYNAME         = 'Y-Offset';
  TRIGGERPARTICLESIZENAME            = 'Partikelgröße';
  TRIGGERPARTICLEVISIBILITYNAME      = 'Sichtbar';
  PROLIGHTNINGLINECOLORNAME          = 'Linienfarbe';
  PROLIGHTNINGBGCOLORNAME            = 'Hintergrundfarbe';
  PROLIGHTNINGROTATIONNAME           = 'Drehung';
  PROLIGHTNINGAUTOROTATIONNAME       = 'Drehungsdiffusität';
  PROLIGHTNINGWIDTHNAME              = 'Breite';
  SMOOTHYLINECOLORNAME               = 'Farbe';
  SMOOTHYLINEPOSITIONNAME            = 'Position';
  SMOOTHYLINEWIDTHNAME               = 'Ausbreitung';
  SMOOTHYLINEVERTICALNAME            = 'Vertikal';
  SMILEYSIZENAME                     = 'Größe';
  SMILEYLEFTEYESIZENAME              = 'Größe Linkes Auge';
  SMILEYRIGHTEYESIZENAME             = 'Größe Rechtes Auge';
  SMILEYMOUTHANGLENAME               = 'Mundwinkel';
  SMILEYMOUTHOPENINGNAME             = 'Mundöffnung';
  SMILEYCOLORNAME                    = 'Farbe';
  SMILEYLEFTEYECOLORNAME             = 'Farbe Linkes Auge';
  SMILEYRIGHTEYECOLORNAME            = 'Farbe Rechtes Auge';
  SMILEYMOUTHCOLORNAME               = 'Mundfarbe';
  SMILEYDATANAME                     = 'Daten';
  SMILEYSCALENAME                    = 'Streckfaktor';
  SMILEYLEFTEYEDATANAME              = 'Daten Linkes Auge';
  SMILEYRIGHTEYEDATANAME             = 'Daten Rechtes Auge';
  SMILEYLEFTEYESCALENAME             = 'Streckfaktor Linkes Auge';
  SMILEYRIGHTEYESCALENAME            = 'Streckfaktor Rechtes Auge';
  SMILEYMOUTHDATANAME                = 'Munddaten';
  SMILEYMOUTHSCALENAME               = 'Mundstreckung';
  SMILEYMOUTHOPENDATANAME            = 'Mundöffnungsdaten';
  SMILEYMOUTHOPENSCALENAME           = 'Mundöffnungsstreckung';

procedure Register;

implementation

{%REGION TNAngle}

procedure NAngleMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas      : IPGL2DCanvas;
  I           : Integer;
  ACenter     : TRealPoint;
  PhiStep     : Real;
  PhiPos      : Real;
  StartPos    : TRealPoint;
  NewPos2     : TRealPoint;
  OldPos2     : TRealPoint;
  ARadius     : Real;
  ASize       : Real;

  AColor      : TVColor;
  ACornerCount: TVInteger;
begin
  with TNAngle(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);

    AColor:=FColor.Value;
    ACornerCount:=FCornerCount.Value;

    if not FVisible.Get then exit;
    if ACornerCount<1 then exit;
    PhiStep:=(2*pi)/ACornerCount;
    if Canvas.Width<Canvas.Height
      then ASize:=Canvas.Width/200.0
      else ASize:=Canvas.Height/200.0;
    ARadius:=FRadius.Get*ASize;
    ACenter:=RealPoint((Canvas.Width/200.0)*FPositionX.Get,(Canvas.Height/200.0)*FPositionY.Get);
    PhiPos:=(FPhi.Get/360.0)*2*pi;

    with StartPos do begin
      X:=(ARadius*sin(PhiPos))+ACenter.X;
      Y:=(ARadius*cos(PhiPos))+ACenter.Y;
    end;
    OldPos2:=StartPos;
    for I:=1 to ACornerCount-1 do begin
      PhiPos+=PhiStep;
      with NewPos2 do begin
        X:=(ARadius*sin(PhiPos))+ACenter.X;
        Y:=(ARadius*cos(PhiPos))+ACenter.Y;
      end;
      Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,AColor);
      OldPos2:=NewPos2;
    end;
    Line(OldPos2.X,OldPos2.Y,StartPos.X,StartPos.Y,AColor);
  end;
end;

procedure CreateNAngle(APrototype: IPVisualisationPrototype); cdecl;
begin
  TNAngle.Create(APrototype);
end;

constructor TNAngle.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@NAngleMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[NANGLECOLORNAME];
  FRadius:=FloatInputs[NANGLERADIUSNAME];
  FCornerCount:=IntegerInputs[NANGLECORNERCOUNTNAME];
  FPositionX:=FloatInputs[NANGLEXNAME];
  FPositionY:=FloatInputs[NANGLEYNAME];
  FPhi:=FloatInputs[NANGLEPHINAME];
  FVisible:=BooleanInputs[NANGLEVISIBILITYNAME];
end;

destructor TNAngle.Destroy;
begin
  FMain.RemoveListener(@NAngleMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  FRadius:=nil;
  FCornerCount:=nil;
  FPositionX:=nil;
  FPositionY:=nil;
  FPhi:=nil;
  FVisible:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TTriggerCircle}

procedure TriggerCircleMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AInnerColor: TVColor;
  ARadius    : TVFloat;

  //TODO: use this!
  function GetALineColor(const AValue: Real): TVColor;
  var
    c1: Integer;
  begin
    c1 := abs(Round(AValue*300));
    if c1>255 then c1:=255;
    c1:=255-c1;
    with TTriggerCircle(Context)
      do Result:=BetaBlend(AInnerColor, FOuterColor.Get,c1);
  end;

var
  ACenter      : TRealPoint;
  I            : Integer;
  PhiStep      : Real;
  PhiPos       : Real;
  StartPos     : TRealPoint;
  NewPos2      : TRealPoint;
  OldPos2      : TRealPoint;
  AActualRadius: Real;
  ACount       : Integer;
  AVal         : Real;
  Canvas       : IPGL2DCanvas;
begin
  with TTriggerCircle(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);

    AInnerColor:=FInnerColor.Value;
    ARadius:=FRadius.Value;

    if not FVisible.Get then exit;
    ACount:=FData.Get.Size;
    if ACount<1 then exit;
    InitMain;
    PhiStep:=(2*pi)/ACount;
    if (Canvas.Width<Canvas.Height) xor (ARadius<0.0)
      then FDrawSize:=Canvas.Width/200.0
      else FDrawSize:=Canvas.Height/200.0;
    AActualRadius:=ARadius*FDrawSize;
    ACenter:=RealPoint((Canvas.Width/200.0)*FPositionX.Get,(Canvas.Height/200.0)*FPositionY.Get);
    PhiPos:=(FPhi.Get/360.0)*2*pi;

    with StartPos do begin
      AVal:=AActualRadius+DoGetBufferItem(0);
      X:=(AVal*sin(PhiPos))+ACenter.X;
      Y:=(AVal*cos(PhiPos))+ACenter.Y;
    end;
    OldPos2:=StartPos;
    for I:=1 to ACount-1 do begin
      PhiPos+=PhiStep;
      with NewPos2 do begin
        AVal:=AActualRadius+DoGetBufferItem(I);
        X:=(AVal*sin(PhiPos))+ACenter.X;
        Y:=(AVal*cos(PhiPos))+ACenter.Y;
      end;
      Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,AInnerColor);
      OldPos2:=NewPos2;
    end;
    Line(OldPos2.X,OldPos2.Y,StartPos.X,StartPos.Y,AInnerColor);
  end;
end;

procedure CreateTriggerCircle(APrototype: IPVisualisationPrototype); cdecl;
begin
  TTriggerCircle.Create(APrototype);
end;

constructor TTriggerCircle.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@TriggerCircleMainCalled, Self, Environment.Thread);
  FInnerColor:=ColorInputs[TRIGGERCIRCLEINNERCOLORNAME];
  FOuterColor:=ColorInputs[TRIGGERCIRCLEOUTERCOLORNAME];
  FRadius:=FloatInputs[TRIGGERCIRCLERADIUSNAME];
  FPositionX:=FloatInputs[TRIGGERCIRCLEXNAME];
  FPositionY:=FloatInputs[TRIGGERCIRCLEYNAME];
  FPhi:=FloatInputs[TRIGGERCIRCLEPHINAME];
  FScale:=FloatInputs[TRIGGERCIRCLESCALENAME];
  FData:=BufferInputs[TRIGGERCIRCLEDATANAME];
  FVisible:=BooleanInputs[TRIGGERCIRCLEVISIBILITYNAME];
end;

destructor TTriggerCircle.Destroy;
begin
  FMain.RemoveListener(@TriggerCircleMainCalled, Self);
  FMain:=nil;
  FInnerColor:=nil;
  FOuterColor:=nil;
  FRadius:=nil;
  FPositionX:=nil;
  FPositionY:=nil;
  FPhi:=nil;
  FScale:=nil;
  FData:=nil;
  FVisible:=nil;
  inherited Destroy;
end;

procedure TTriggerCircle.InitMain;
begin
  //do nothing
end;

function TTriggerCircle.DoGetBufferItem(AIndex: Integer): Real;
begin
  Result:=FData.Get[AIndex]*FScale.Get*DrawSize;
end;

{%ENDREGION}
{%REGION TSmoothedTriggerCircle}

procedure CreateSmoothedTriggerCircle(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSmoothedTriggerCircle.Create(APrototype);
end;

procedure TSmoothedTriggerCircle.InitMain;
var
  ACount: Integer;
begin
  ACount:=FData.Get.Size;
  FAdjustBuf:=(FData.Get[0]-FData.Get[ACount-1])/ACount;
end;

function TSmoothedTriggerCircle.DoGetBufferItem(AIndex: Integer): Real;
begin
  Result:=(FData.Get[AIndex]+FAdjustBuf*AIndex)*FScale.Get*DrawSize;
end;

{%ENDREGION}
{%REGION TRain}

procedure RainMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas                                : IPGL2DCanvas;
  LVec2                                 : TRealPoint;

  AColor1, AColor2                      : TVColor;
  ARainLength, ARainDist, AScale, ASpeed: TVFloat;
  AData                                 : TVBuffer;

const
  _DoSteps     = 15;
  _DoColorSteps= 255/(_DoSteps-1);

  function GetALineColor(const AValue: Real): TVColor;
  var
    c1: Integer;
  begin
    c1 := abs(Round(AValue));
    if c1>255 then c1:=255;
    c1:=255-c1;
    with TRain(Context)
      do Result:=BetaBlend(AColor1, AColor2, c1);
  end;

  //TODO: replace by opengl functionality
  procedure DoDrawRainLine(const AX,AY: Real);
  var
    I         : Integer;
    APos,APos2: TRealPoint;
  begin
    APos:=RealPoint(AX,AY);
    for I:=0 to _DoSteps-1 do begin
      with APos2 do begin
        X:=APos.X+LVec2.X;
        Y:=APos.Y+LVec2.Y;
      end;
      Line(APos.X,APos.Y,APos2.X,APos2.Y,GetALineColor(I*_DoColorSteps));
      APos:=APos2;
    end;
  end;

var
  I,J           : Integer;
  PhiPos        : Real;
  StartPos      : TRealPoint;
  AStep         : Real;
  ASize         : Real;
  ACount        : Integer;
  AVal          : Real;
  AWidth,AHeight: Real;
  ABPhi         : Real;
  AYStep,AXStep : Real;
  LengthVector  : TRealPoint;
  DistVector    : TRealPoint;
  MaxDraw       : Integer;
  AVR,AVP       : Real;
  ARainSize     : Real;
begin
  with TRain(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);

    AColor1:=FColor1.Value;
    AColor2:=FColor2.Value;
    AData:=FData.Value;
    ARainLength:=FRainLength.Value;
    ARainDist:=FRainDist.Value;
    AScale:=FScale.Value;
    ASpeed:=FSpeed.Value;

    if not FVisible.Get then exit;
    ACount:=AData.Size;
    if ACount<1 then exit;
    {if Canvas.Width<Canvas.Height
      then ASize2:=Canvas.Width/200.0
      else ASize2:=Canvas.Height/200.0;}
    ASize:=sqrt(sqr(Canvas.Width)+sqr(Canvas.Height));
    PhiPos:=(FPhi.Get/360.0)*2*pi;
    ABPhi:=Arcsin(Canvas.Height/ASize);

    AStep:=Frac(PhiPos/(2*pi)); //AStep als Temporäre Variable
    if PhiPos>=0 then begin
      if (AStep<=1/4) or ((AStep>=1/2) and (AStep<=3/4)) then begin
        AWidth:=Abs(cos(PhiPos-ABPhi)*ASize);
        AHeight:=Abs(cos(PhiPos-((pi/2)-ABPhi){ABPhi-PhiPos})*ASize);
      end else begin
        AWidth:=Abs(sin(PhiPos-((pi/2)-ABPhi){ABPhi-PhiPos})*ASize);
        AHeight:=Abs(sin(PhiPos-ABPhi)*ASize);
      end;
    end else begin
      if ((AStep<=-1/4) and (AStep>=-1/2)) or ((AStep<=-3/2) and (AStep>=-1)) then begin
        AWidth:=Abs(cos(PhiPos-ABPhi)*ASize);
        AHeight:=Abs(cos(PhiPos-((pi/2)-ABPhi){ABPhi-PhiPos})*ASize);
      end else begin
        AWidth:=Abs(sin(PhiPos-((pi/2)-ABPhi){ABPhi-PhiPos})*ASize);
        AHeight:=Abs(sin(PhiPos-ABPhi)*ASize);
      end;
    end;
    ARainSize:=ARainLength+ARainDist;
    AStep:=100.0/(ACount{AParams.RDist}-1);
    AXStep:=AWidth/100.0;
    AYStep:=AHeight/100.0;

    LengthVector:=RealPoint(ARainLength*AYStep*sin(PhiPos), ARainLength*AYStep*cos(PhiPos));
    DistVector:=RealPoint(ARainDist*AYStep*sin(PhiPos), ARainDist*AYStep*cos(PhiPos));
    LVec2:=LengthVector/_DoSteps;

    MaxDraw:=Trunc(100.0/(ARainLength+ARainDist))+2;

    for I:=0 to ACount-1 do begin
      with StartPos do begin
        AVal:=(((Frac((AData[I]*AScale)/(ARainSize))*ARainSize)-ARainSize+FRainPos)*AYStep);

        X:=(I*AStep*AXStep)-(AWidth/2);
        Y:=AVal-(AHeight/2);
        AVR:=sqrt(sqr(X)+sqr(Y));
        AVP:=Arctan2(Y,X);
        X:=((AVR*cos(AVP-PhiPos)));
        Y:=((AVR*sin(AVP-PhiPos)));
      end;
      for J:=0 to MaxDraw-1 do begin
        DoDrawRainLine(StartPos.X,StartPos.Y);
        StartPos+=LengthVector+DistVector;
        {with StartPos do begin
          X+=LengthVector.X+DistVector.X;
          Y+=LengthVector.Y+DistVector.Y;
        end;}
      end;
    end;
    FRainPos+=ASpeed;
    if FRainPos>=ARainLength+ARainDist
      then FRainPos-=ARainLength+ARainDist;
  end;
end;

procedure CreateRain(APrototype: IPVisualisationPrototype); cdecl;
begin
  TRain.Create(APrototype);
end;

constructor TRain.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@RainMainCalled, Self, Environment.Thread);
  FColor1:=ColorInputs[RAINCOLOR1NAME];
  FColor2:=ColorInputs[RAINCOLOR2NAME];
  FRainLength:=FloatInputs[RAINLENGTHNAME];
  FRainDist:=FloatInputs[RAINDISTNAME];
  FSpeed:=FloatInputs[RAINSPEEDNAME];
  FPhi:=FloatInputs[RAINPHINAME];
  FScale:=FloatInputs[RAINSCALENAME];
  FData:=BufferInputs[RAINDATANAME];
  FVisible:=BooleanInputs[RAINVISIBILITYNAME];

  FRainPos:=0.0;
end;

destructor TRain.Destroy;
begin
  FMain.RemoveListener(@RainMainCalled, Self);
  FMain:=nil;
  FColor1:=nil;
  FColor2:=nil;
  FRainLength:=nil;
  FRainDist:=nil;
  FSpeed:=nil;
  FPhi:=nil;
  FScale:=nil;
  FData:=nil;
  FVisible:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TTriggerParticles}

procedure TriggerParticlesMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  Canvas                                                            : IPGL2DCanvas;
  AMax,ABeta                                                        : Cardinal;
  I                                                                 : Integer;
  AVal,AStep                                                        : TRealPoint;
  AColor                                                            : TVColor;
  SizeOffset                                                        : Real;

  AXData, AYData                                                    : TVBuffer;
  AXScale, AYScale, AXOffset, AYOffset                              : TVFloat;
  ARightBottomColor, ALeftBottomColor, ARightTopColor, ALeftTopColor: TVColor;
const
  BetaStep = 255.0/200.0;
begin
  with TTriggerParticles(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;
    Canvas:=IPGL2DCanvas(Environment.Canvas);

    AXData:=FXData.Value;
    AYData:=FYData.Value;
    AXScale:=FXScale.Value;
    AYScale:=FYScale.Value;
    AXOffset:=FXOffset.Value;
    AYOffset:=FYOffset.Value;
    ARightBottomColor:=FRightBottomColor.Value;
    ALeftBottomColor:=FLeftBottomColor.Value;
    ARightTopColor:=FRightTopColor.Value;
    ALeftTopColor:=FLeftTopColor.Value;

    AMax:=AXData.Size;
    if AMax>AYData.Size
      then AMax:=AYData.Size;
    AStep:=RealPoint(Canvas.Width/200.0,Canvas.Height/200.0);
    SizeOffset:=FSize.Get/2.0;
    for I:=0 to AMax-1 do begin
      AVal:=RealPoint((AXData[I]*AXScale)+AXOffset,-((AYData[I]*AYScale)+AYOffset));
      ABeta:=Round(HLCut(AVal.X+100.0,200.0)*BetaStep);
      AColor:=BetaBlend(BetaBlend(ARightBottomColor, ALeftBottomColor, ABeta),BetaBlend(ARightTopColor, ALeftTopColor, ABeta),Round(HLCut(AVal.Y+100.0,200.0)*BetaStep));
      AVal.X:=(AVal.X*AStep.X);
      AVal.Y:=(AVal.Y*AStep.Y);
      FillRect(AVal.X-SizeOffset,AVal.Y-SizeOffset,AVal.X+SizeOffset,AVal.Y+SizeOffset,AColor);
    end;
  end;
end;

procedure CreateTriggerParticles(APrototype: IPVisualisationPrototype); cdecl;
begin
  TTriggerParticles.Create(APrototype);
end;

constructor TTriggerParticles.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@TriggerParticlesMainCalled, Self, Environment.Thread);
  FLeftTopColor:=ColorInputs[TRIGGERPARTICLELEFTTOPCOLORNAME];
  FRightTopColor:=ColorInputs[TRIGGERPARTICLERIGHTTOPCOLORNAME];
  FLeftBottomColor:=ColorInputs[TRIGGERPARTICLELEFTBOTTOMCOLORNAME];
  FRightBottomColor:=ColorInputs[TRIGGERPARTICLERIGHTBOTTOMCOLORNAME];
  FXData:=BufferInputs[TRIGGERPARTICLEDATAXNAME];
  FYData:=BufferInputs[TRIGGERPARTICLEDATAYNAME];
  FXScale:=FloatInputs[TRIGGERPARTICLESCALEXNAME];
  FYScale:=FloatInputs[TRIGGERPARTICLESCALEYNAME];
  FXOffset:=FloatInputs[TRIGGERPARTICLEOFFSETXNAME];
  FYOffset:=FloatInputs[TRIGGERPARTICLEOFFSETYNAME];
  FSize:=FloatInputs[TRIGGERPARTICLESIZENAME];
  FVisible:=BooleanInputs[TRIGGERPARTICLEVISIBILITYNAME];
end;

destructor TTriggerParticles.Destroy;
begin
  FMain.RemoveListener(@TriggerParticlesMainCalled, Self);
  FMain:=nil;
  FLeftTopColor:=nil;
  FRightTopColor:=nil;
  FLeftBottomColor:=nil;
  FRightBottomColor:=nil;
  FXData:=nil;
  FYData:=nil;
  FXScale:=nil;
  FYScale:=nil;
  FXOffset:=nil;
  FYOffset:=nil;
  FSize:=nil;
  FVisible:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TProLightning}

procedure ProLightningMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  ARot,ARot2 : Real;
  ALineColor : TVColor;
const
  //PiToDegree = 180.0/Pi;
  DegreeToPi = Pi/180.0;
begin
  with TProLightning(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;

    ALineColor:=FLineColor.Value;

    if Random>0.75
      then FRot+=PhiCut(Random*FAutoRotation.Get*DegreeToPi);
    ARot:=FRot+(FRotation.Get*DegreeToPi);
    ARot2:=ARot+(FWidth.Get*DegreeToPi);
    //Draw
    Line(0.0, 0.0, FR*Sin(ARot), FR*Cos(ARot), ALineColor);
    Line(0.0, 0.0, FR*Sin(ARot2), FR*Cos(ARot2), ALineColor);
  end;
end;

procedure CreateProLightning(APrototype: IPVisualisationPrototype); cdecl;
begin
  TProLightning.Create(APrototype);
end;

constructor TProLightning.Create(APrototype: IPVisualisationPrototype);
var
  Canvas: IPGL2DCanvas;
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@ProLightningMainCalled, Self, Environment.Thread);
  FLineColor:=ColorInputs[PROLIGHTNINGLINECOLORNAME];
  FBGColor:=ColorInputs[PROLIGHTNINGBGCOLORNAME];
  FRotation:=FloatInputs[PROLIGHTNINGROTATIONNAME];
  FAutoRotation:=FloatInputs[PROLIGHTNINGAUTOROTATIONNAME];
  FWidth:=FloatInputs[PROLIGHTNINGWIDTHNAME];

  FRot:=0.0;
  if Environment.Canvas.&Type=cGL2DCanvas then begin
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    if Canvas.Width<Canvas.Height
      then FR:=Canvas.Width
      else FR:=Canvas.Height;
  end else FR:=1.0;
end;

destructor TProLightning.Destroy;
begin
  FMain.RemoveListener(@ProLightningMainCalled, Self);
  FMain:=nil;
  FLineColor:=nil;
  FBGColor:=nil;
  FRotation:=nil;
  FAutoRotation:=nil;
  FWidth:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TSmoothyLine}

procedure SmoothyLineWidthChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TSmoothyLine(Context).DoWidthChanged(IChangedFloat(SenderData).Value);
end;

procedure SmoothyLineOrientationChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TSmoothyLine(Context).DoOrientationChanged(IChangedBoolean(SenderData).Value);
end;

procedure SmoothyLineMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TSmoothyLine(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;

    //Wenn Alpha 0 ist würde man sowieso nichts sehen...
    if AlphaComponent(FColor.Get)=0 then exit;
    FDrawProc(FScale*(FPosition.Get+100.0));
  end;
end;

procedure CreateSmoothyLine(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSmoothyLine.Create(APrototype);
end;

procedure TSmoothyLine.DoWidthChanged(AWidth: TVFloat);
begin
  FLineOffset:=AWidth*FScale;
end;

procedure TSmoothyLine.DoOrientationChanged(AVertical: TVBoolean);
begin
  if AVertical then begin
    FDrawProc:=@DrawSmoothyLine_Vertical;
    if Environment.Canvas.&Type=cGL2DCanvas
      then FScale:=IPGL2DCanvas(Environment.Canvas).Width/200.0
      else FScale:=1.0;
  end else begin
    FDrawProc:=@DrawSmoothyLine_Horizontal;
    if Environment.Canvas.&Type=cGL2DCanvas
      then FScale:=IPGL2DCanvas(Environment.Canvas).Height/200.0
      else FScale:=1.0;
  end;
  DoWidthChanged(FWidth.Value);
end;

constructor TSmoothyLine.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@SmoothyLineMainCalled, Self, Environment.Thread);
  FColor:=ColorInputs[SMOOTHYLINECOLORNAME];
  FPosition:=FloatInputs[SMOOTHYLINEPOSITIONNAME];
  FWidth:=FloatInputs[SMOOTHYLINEWIDTHNAME];
  FWidth.AddListener(@SmoothyLineWidthChanged, Self, Environment.Thread);
  FVertical:=BooleanInputs[SMOOTHYLINEVERTICALNAME];
  FVertical.AddListener(@SmoothyLineOrientationChanged, Self, Environment.Thread);

  DoOrientationChanged(FVertical.Value);
end;

destructor TSmoothyLine.Destroy;
begin
  FMain.RemoveListener(@SmoothyLineMainCalled, Self);
  FMain:=nil;
  FColor:=nil;
  FPosition:=nil;
  FWidth.RemoveListener(@SmoothyLineWidthChanged, Self);
  FWidth:=nil;
  FVertical.RemoveListener(@SmoothyLineOrientationChanged, Self);
  FVertical:=nil;
  inherited Destroy;
end;

procedure TSmoothyLine.DrawSmoothyLine_Vertical(APos: Real);
var
  ASmoothColor : TVColor;
  Canvas       : IPGL2DCanvas;
  ALeft, ATop  : Real;

  AColor       : TVColor;
begin
  //only called if Canvas.&Type=cGL2DCanvas, thus, no check is needed
  Canvas:=IPGL2DCanvas(Environment.Canvas);
  ALeft:=-Canvas.Width/2.0;
  ATop:=-Canvas.Height/2.0;

  AColor:=FColor.Value;

  ASmoothColor:=AColor and $00FFFFFF;
  FillRect(ALeft+APos-FLineOffset, ATop, ALeft+APos, ATop+Canvas.Height, ASmoothColor, AColor, ASmoothColor, AColor);
  FillRect(ALeft+APos, ATop, ALeft+APos+FLineOffset, ATop+Canvas.Height, AColor, ASmoothColor, AColor, ASmoothColor);
end;

procedure TSmoothyLine.DrawSmoothyLine_Horizontal(APos: Real);
var
  ASmoothColor : TVColor;
  Canvas       : IPGL2DCanvas;
  ALeft, ATop  : Real;

  AColor       : TVColor;
begin
  //only called if Canvas.&Type=cGL2DCanvas, thus, no check is needed
  Canvas:=IPGL2DCanvas(Environment.Canvas);
  ALeft:=-Canvas.Width/2.0;
  ATop:=-Canvas.Height/2.0;

  AColor:=FColor.Value;

  ASmoothColor:=AColor and $00FFFFFF;
  FillRect(ALeft, ATop+APos-FLineOffset, ALeft+Canvas.Width, ATop+APos, ASmoothColor, ASmoothColor, AColor, AColor);
  FillRect(ALeft, ATop+APos, ALeft+Canvas.Width, ATop+APos+FLineOffset, AColor, AColor, ASmoothColor, ASmoothColor);
end;

{%ENDREGION}
{%REGION TSmiley}

procedure SmileyMainCalled(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TSmiley(Context) do begin
    if Environment.Canvas.&Type<>cGL2DCanvas
      then exit;

    FDrawBody(ZEROCENTER,FBodySize,FBodyScale,FColor,FData);
    FDrawLeftEye(FLeftEyePos,FLeftEyeSize.Get*FEyeRel,FLeftEyeScale.Get*FEyeRel,FLeftEyeColor,FLeftEyeData);
    FDrawRightEye(FRightEyePos,FRightEyeSize.Get*FEyeRel,FRightEyeScale.Get*FEyeRel,FRightEyeColor,FRightEyeData);
  end;
end;

procedure SmileyBodySizeChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TSmiley(Context).DoBodySizeChanged(IChangedFloat(SenderData).Value);
end;

procedure SmileyDataChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TSmiley(Context)
    do FDrawBody:=DrawCircle[IChangedBuffer(Sender).Value.Size>0];
end;

procedure SmileyLeftEyeDataChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TSmiley(Context)
    do FDrawLeftEye:=DrawCircle[IChangedBuffer(Sender).Value.Size>0];
end;

procedure SmileyRightEyeDataChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
    with TSmiley(Context)
    do FDrawRightEye:=DrawCircle[IChangedBuffer(Sender).Value.Size>0];
end;

procedure SmileyMouthDataChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  //TODO: implement this
end;

procedure SmileyMouthOpenDataChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  //TODO: implement this
end;

procedure CreateSmiley(APrototype: IPVisualisationPrototype); cdecl;
begin
  TSmiley.Create(APrototype);
end;

procedure TSmiley.DoBodySizeChanged(ASize: TVFloat);
var
  AEyeOffset: Real;
const
  _EyeBodyDist = 0.4;
  _EyeBodyDistI= 1.0-_EyeBodyDist;
begin
  FBodySize:=FBodyRel*ASize;
  FBodyScale:=FBodyRel*FScale.Get;

  FEyeRel:=FBodySize*_EyeBodyDist/100.0; //100 because of %

  AEyeOffset:=sqrt(sqr(FBodySize*_EyeBodyDistI)/2);
  FLeftEyePos:=RealPoint(-AEyeOffset,-AEyeOffset);
  FRightEyePos:=RealPoint(AEyeOffset,-AEyeOffset);
end;

constructor TSmiley.Create(APrototype: IPVisualisationPrototype);
var
  Canvas     : IPGL2DCanvas;
begin
  inherited Create(APrototype);
  FMain:=CallInputs[MAININPUTNAME];
  FMain.AddListener(@SmileyMainCalled, Self, Environment.Thread);
  FSize:=FloatInputs[SMILEYSIZENAME];
  FSize.AddListener(@SmileyBodySizeChanged, Self, Environment.Thread);
  FLeftEyeSize:=FloatInputs[SMILEYLEFTEYESIZENAME];
  FRightEyeSize:=FloatInputs[SMILEYRIGHTEYESIZENAME];
  FMouthAngle:=FloatInputs[SMILEYMOUTHANGLENAME];
  FMouthOpen:=FloatInputs[SMILEYMOUTHOPENINGNAME];
  FColor:=ColorInputs[SMILEYCOLORNAME];
  FLeftEyeColor:=ColorInputs[SMILEYLEFTEYECOLORNAME];
  FRightEyeColor:=ColorInputs[SMILEYRIGHTEYECOLORNAME];
  FMouthColor:=ColorInputs[SMILEYMOUTHCOLORNAME];
  FData:=BufferInputs[SMILEYDATANAME];
  FData.AddListener(@SmileyDataChanged, Self, Environment.Thread);
  FScale:=FloatInputs[SMILEYSCALENAME];
  FLeftEyeData:=BufferInputs[SMILEYLEFTEYEDATANAME];
  FLeftEyeData.AddListener(@SmileyLeftEyeDataChanged, Self, Environment.Thread);
  FLeftEyeScale:=FloatInputs[SMILEYLEFTEYESCALENAME];
  FRightEyeData:=BufferInputs[SMILEYRightEYEDATANAME];
  FRightEyeData.AddListener(@SmileyRightEyeDataChanged, Self, Environment.Thread);
  FRightEyeScale:=FloatInputs[SMILEYRIGHTEYESCALENAME];
  FMouthData:=BufferInputs[SMILEYMOUTHDATANAME];
  FMouthData.AddListener(@SmileyMouthDataChanged, Self, Environment.Thread);
  FMouthScale:=FloatInputs[SMILEYMOUTHSCALENAME];
  FMouthOpenData:=BufferInputs[SMILEYMOUTHOPENDATANAME];
  FMouthOpenData.AddListener(@SmileyMouthOpenDataChanged, Self, Environment.Thread);
  FMouthOpenScale:=FloatInputs[SMILEYMOUTHOPENSCALENAME];

  DrawCircle[false]:=@DrawDefaultCircle;
  DrawCircle[true]:=@DrawDataCircle;

  if Environment.Canvas.&Type=cGL2DCanvas then begin
    Canvas:=IPGL2DCanvas(Environment.Canvas);
    if Canvas.Width>=Canvas.Height
      then FBodyRel:=(Canvas.Height/200.0) //2*100: 2 because of + or -; 100 because of %
      else FBodyRel:=(Canvas.Width/200.0);
  end else FBodyRel:=0.02;//1.0/200.0 <- an arbitrary chosen number (the 1.0)

  DoBodySizeChanged(FSize.Value);

  FDrawBody:=DrawCircle[FData.Get.Size>0];
  FDrawLeftEye:=DrawCircle[FLeftEyeData.Get.Size>0];
  FDrawRightEye:=DrawCircle[FRightEyeData.Get.Size>0];
end;

destructor TSmiley.Destroy;
begin
  FMain.RemoveListener(@SmileyMainCalled, Self);
  FMain:=nil;
  FSize.RemoveListener(@SmileyBodySizeChanged, Self);
  FSize:=nil;
  FLeftEyeSize:=nil;
  FRightEyeSize:=nil;
  FMouthAngle:=nil;
  FMouthOpen:=nil;
  FColor:=nil;
  FLeftEyeColor:=nil;
  FRightEyeColor:=nil;
  FMouthColor:=nil;
  FData.RemoveListener(@SmileyDataChanged, Self);
  FData:=nil;
  FScale:=nil;
  FLeftEyeData.RemoveListener(@SmileyLeftEyeDataChanged, Self);
  FLeftEyeData:=nil;
  FLeftEyeScale:=nil;
  FRightEyeData.RemoveListener(@SmileyRightEyeDataChanged, Self);
  FRightEyeData:=nil;
  FRightEyeScale:=nil;
  FMouthData.RemoveListener(@SmileyMouthDataChanged, Self);
  FMouthData:=nil;
  FMouthScale:=nil;
  FMouthOpenData.RemoveListener(@SmileyMouthOpenDataChanged, Self);
  FMouthOpenData:=nil;
  FMouthOpenScale:=nil;
  inherited Destroy;
end;

procedure TSmiley.DrawDefaultCircle(Pos: TRealPoint; R,Scale: Real; Color: TVColor; Buf: TVBuffer);
var
  I                 : Integer;
  AIncrement,APhi   : Real;
  APos,APos2        : TRealPoint;
begin
  (*ASteps:=Trunc(2*Pi*R*PointDist);*)
  if IsZero(R) then exit;
  //AR:=R/100.0;
  AIncrement:=1/R;(*2*Pi/ASteps*)

  APhi:=AIncrement;
  APos:=RealPoint(Pos.X,Pos.Y+R);(*R*Sin(APhi),R*Cos(APhi)*)
  for I:=1 to Trunc(2*Pi*R) do begin
    APos2:=RealPoint(R*Sin(APhi)+Pos.X,R*Cos(APhi)+Pos.Y);
    APhi+=AIncrement;
    Line(APos.X,APos.Y,APos2.X,APos2.Y,Color);
    APos:=APos2;
  end;
  Line(APos.X,APos.Y,Pos.X,Pos.Y+R,Color);
end;

procedure TSmiley.DrawDataCircle(Pos: TRealPoint; R,Scale: Real; Color: TVColor; Buf: TVBuffer);
var
  I,ASteps            : Integer;
  AIncrement,APhi,AR  : Real;
  APos,APos2          : TRealPoint;

  function GetRVal(Index: Integer): Real; inline;
  begin
    Result:=R+Buf[Index]*Scale;
  end;

begin
  ASteps:=Buf.Size;
  AIncrement:=(2*Pi)/ASteps;

  APhi:=AIncrement;
  APos:=RealPoint(Pos.X,GetRVal(0)+Pos.Y);(*GetRVal(0)*Sin(APhi),GetRVal(0)*Cos(APhi)*)
  for I:=1 to ASteps-1 do begin
    AR:=GetRVal(I);
    APos2:=RealPoint(AR*Sin(APhi)+Pos.X,AR*Cos(APhi)+Pos.Y);
    APhi+=AIncrement;
    Line(APos.X,APos.Y,APos2.X,APos2.Y,Color);
    APos:=APos2;
  end;
  Line(APos.X,APos.Y,Pos.X,GetRVal(0)+Pos.Y,Color);
end;

{%ENDREGION}
{%REGION Misc}

procedure Register;
begin
  with PresetUtil do begin
    RegisterVis(VIDNANGLE, @CreateNAngle);
    with CreatePreset('n-Angle', VIDNANGLE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, NANGLECOLORNAME, $FFFF0000);
      AddInput(This, NANGLERADIUSNAME, 50.0);
      AddInput(This, NANGLECORNERCOUNTNAME, 6);
      AddInput(This, NANGLEXNAME, 0.0);
      AddInput(This, NANGLEYNAME, 0.0);
      AddInput(This, NANGLEPHINAME, 0.0);
      AddInput(This, NANGLEVISIBILITYNAME, true);
    end;
    RegisterVis(VIDTRIGGERCIRCLE, @CreateTriggerCircle);
    with CreatePreset('Trigger Circle', VIDTRIGGERCIRCLE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Triggered');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, TRIGGERCIRCLEINNERCOLORNAME, $FFFF0000);
      AddInput(This, TRIGGERCIRCLEOUTERCOLORNAME, TVColor($7FFF0000));
      AddInput(This, TRIGGERCIRCLERADIUSNAME, 50.0);
      AddInput(This, TRIGGERCIRCLEXNAME, 0.0);
      AddInput(This, TRIGGERCIRCLEYNAME, 0.0);
      AddInput(This, TRIGGERCIRCLEPHINAME, 0.0);
      AddInput(This, TRIGGERCIRCLESCALENAME, 1.0);
      AddInput(This, TRIGGERCIRCLEDATANAME, DEFAULTBUFFER);
      AddInput(This, TRIGGERCIRCLEVISIBILITYNAME, true);
    end;
    RegisterVis(VIDSMOOTHEDTRIGGERCIRCLE, @CreateSmoothedTriggerCircle);
    with CreatePreset('Smoothed Trigger Circle', VIDSMOOTHEDTRIGGERCIRCLE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Triggered');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, TRIGGERCIRCLEINNERCOLORNAME, $FFFF0000);
      AddInput(This, TRIGGERCIRCLEOUTERCOLORNAME, TVColor($7FFF0000));
      AddInput(This, TRIGGERCIRCLERADIUSNAME, 50.0);
      AddInput(This, TRIGGERCIRCLEXNAME, 0.0);
      AddInput(This, TRIGGERCIRCLEYNAME, 0.0);
      AddInput(This, TRIGGERCIRCLEPHINAME, 0.0);
      AddInput(This, TRIGGERCIRCLESCALENAME, 1.0);
      AddInput(This, TRIGGERCIRCLEDATANAME, DEFAULTBUFFER);
      AddInput(This, TRIGGERCIRCLEVISIBILITYNAME, true);
    end;
    RegisterVis(VIDRAIN, @CreateRain);
    with CreatePreset('Rain', VIDRAIN) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Triggered');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, RAINCOLOR1NAME, $FF0000FF);
      AddInput(This, RAINCOLOR2NAME, TVColor($00007FFF));
      AddInput(This, RAINLENGTHNAME, 20.0);
      AddInput(This, RAINDISTNAME, 0.0);
      AddInput(This, RAINSPEEDNAME, 1.0);
      AddInput(This, RAINPHINAME, 0.0);
      AddInput(This, RAINSCALENAME, 1.0);
      AddInput(This, RAINDATANAME, DEFAULTBUFFER);
      AddInput(This, RAINVISIBILITYNAME, true);
    end;
    RegisterVis(VIDTRIGGERPARTICLES, @CreateTriggerParticles);
    with CreatePreset('Trigger Particles', VIDTRIGGERPARTICLES) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Triggered');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, TRIGGERPARTICLELEFTTOPCOLORNAME, $FFFF0000);
      AddInput(This, TRIGGERPARTICLERIGHTTOPCOLORNAME, $FF00FF00);
      AddInput(This, TRIGGERPARTICLELEFTBOTTOMCOLORNAME, $FF0000FF);
      AddInput(This, TRIGGERPARTICLERIGHTBOTTOMCOLORNAME, $FFFFFF00);
      AddInput(This, TRIGGERPARTICLEDATAXNAME, DEFAULTBUFFER);
      AddInput(This, TRIGGERPARTICLEDATAYNAME, DEFAULTBUFFER);
      AddInput(This, TRIGGERPARTICLESCALEXNAME, 1.0);
      AddInput(This, TRIGGERPARTICLESCALEYNAME, 1.0);
      AddInput(This, TRIGGERPARTICLEOFFSETXNAME, 0.0);
      AddInput(This, TRIGGERPARTICLEOFFSETYNAME, 0.0);
      AddInput(This, TRIGGERPARTICLESIZENAME, 4.0);
      AddInput(This, TRIGGERPARTICLEVISIBILITYNAME, true);
    end;
    RegisterVis(VIDPROLIGHTNING, @CreateProLightning);
    with CreatePreset('Pro Lightning', VIDPROLIGHTNING) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Ugly');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, PROLIGHTNINGLINECOLORNAME, $FFFFFF00);
      AddInput(This, PROLIGHTNINGBGCOLORNAME, TVColor($77FFFF00));
      AddInput(This, PROLIGHTNINGROTATIONNAME, 0.0);
      AddInput(This, PROLIGHTNINGAUTOROTATIONNAME, 3.0);
      AddInput(This, PROLIGHTNINGWIDTHNAME, 42.0);
    end;
    RegisterVis(VIDSMOOTHYLINE, @CreateSmoothyLine);
    with CreatePreset('Smoothy Line', VIDSMOOTHYLINE) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Simple');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, SMOOTHYLINECOLORNAME, $FF00FF00);
      AddInput(This, SMOOTHYLINEPOSITIONNAME, 0.0);
      AddInput(This, SMOOTHYLINEWIDTHNAME, 10.0);
      AddInput(This, SMOOTHYLINEVERTICALNAME, true);
    end;
    RegisterVis(VIDSMILEY, @CreateSmiley);
    with CreatePreset('Smiley', VIDSMILEY) do begin
      AddTag(TAGLISTED);
      AddTag('Layers.Triggered');
      AddTag(TAGPREDEFINED);
      AddInput(This, MAININPUTNAME);
      AddInput(This, SMILEYSIZENAME, 50.0);
      AddInput(This, SMILEYLEFTEYESIZENAME, 100.0);
      AddInput(This, SMILEYRIGHTEYESIZENAME, 100.0);
      AddInput(This, SMILEYMOUTHANGLENAME, 100.0);
      AddInput(This, SMILEYMOUTHOPENINGNAME, 0.0);
      AddInput(This, SMILEYCOLORNAME, $FFFFFF00);
      AddInput(This, SMILEYLEFTEYECOLORNAME, $FFFFFF00);
      AddInput(This, SMILEYRIGHTEYECOLORNAME, $FFFFFF00);
      AddInput(This, SMILEYMOUTHCOLORNAME, $FFFFFF00);
      AddInput(This, SMILEYDATANAME, DEFAULTBUFFER);
      AddInput(This, SMILEYSCALENAME, 1.0);
      AddInput(This, SMILEYLEFTEYEDATANAME, DEFAULTBUFFER);
      AddInput(This, SMILEYLEFTEYESCALENAME, 1.0);
      AddInput(This, SMILEYRIGHTEYEDATANAME, DEFAULTBUFFER);
      AddInput(This, SMILEYRIGHTEYESCALENAME, 1.0);
      AddInput(This, SMILEYMOUTHDATANAME, DEFAULTBUFFER);
      AddInput(This, SMILEYMOUTHSCALENAME, 1.0);
      AddInput(This, SMILEYMOUTHOPENDATANAME, DEFAULTBUFFER);
      AddInput(This, SMILEYMOUTHOPENSCALENAME, 1.0);
    end;
  end;
end;

{%ENDREGION}

end.

