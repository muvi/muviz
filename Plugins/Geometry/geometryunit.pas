unit GeometryUnit;

{$mode objfpc}{$H+}

interface

uses
  VisType, SpectrumData, VPBuffers, Classes, GraphX32, MainType, AdvCoord, Math,
  AdvFunc;

procedure DrawNAngle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawTrigCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawTrigSmoothCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawTrigRain(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitTrigRain(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawTrigPart(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitProLightning(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawProLightning(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure ChangeSmoothyLineWidth(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeSmoothyLineOrientation(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawSmoothyLine(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawSmoothyLine_Vertical(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure DrawSmoothyLine_Horizontal(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure Smiley_DrawDefaultCircle(Dest: IMCanvas; Pos: TRealPoint; R,Scale: Real; Color: vpColor; Buf: vpRealBuffer);
procedure Smiley_DrawDataCircle(Dest: IMCanvas; Pos: TRealPoint; R,Scale: Real; Color: vpColor; Buf: vpRealBuffer);
procedure DrawSmiley(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitSmiley(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure FreeSmiley(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeSmileyBodySize(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeSmileyData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeSmileyLeftEyeData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeSmileyRightEyeData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeSmileyMouthData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeSmileyMouthOpenData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

type
  TNAngleParams     = packed record
    Radius : vpReal;
    Angles : vpInt;
    PosX   : vpReal;
    PosY   : vpReal;
    Phi    : vpReal;
    Visible: vpBool;
  end;

  TTrigCircleParams = packed record
    Radius : vpReal;
    PosX   : vpReal;
    PosY   : vpReal;
    Phi    : vpReal;
    Scale  : vpReal;
    Data   : vpRealBuffer;
    Visible: vpBool;
  end;

  TTrigRainParams = packed record
    RLength: vpReal;
    RDist  : vpReal;
    Speed  : vpReal;
    Phi    : vpReal;
    Scale  : vpReal;
    Data   : vpRealBuffer;
    Visible: vpBool;
  end;

  TRainWS         = record
    RainPos: vpReal;
  end;

  TTrigPartParams = packed record
    C4                                : vpColor;
    XData,YData                       : vpRealBuffer;
    XScale,YScale,XOffset,YOffset,Size: vpReal;
    Visible                           : vpBool;
  end;

  TProLightParams = packed record
    LineColor,BGColor     : vpColor;
    Rotation,AutoRot,Width: vpReal;
  end;

  TProLightWS     = record
    Rot   : vpReal;
    Center: TRealPoint;
    R     : Real;
  end;

  TSmileyParams   = packed record
    Size,LeftEye,RightEye,MouthAngle,MouthOpen : vpReal;
    Color,LeftEyeColor,RightEyeColor,MouthColor: vpColor;
    Data                                       : vpRealBuffer;
    Scale                                      : vpReal;
    LeftEyeData                                : vpRealBuffer;
    LeftEyeScale                               : vpReal;
    RightEyeData                               : vpRealBuffer;
    RightEyeScale                              : vpReal;
    MouthData                                  : vpRealBuffer;
    MouthScale                                 : vpReal;
    MouthOpenData                              : vpRealBuffer;
    MouthOpenScale                             : vpReal;
  end;

  TSmileyCircleDraw  = procedure (Dest: IMCanvas; Pos: TRealPoint; R,Scale: Real; Color: vpColor; Buf: vpRealBuffer);
  TSmileyWS          = record
    Center,LeftEyePos,RightEyePos                                         : TRealPoint;
    BodyRel,EyeRel                                                        : Real;
    BodySize,{LeftEyeSize,RightEyeSize,}BodyScale{,LeftEyeScale,RightEyeScale}: Real;
    DrawBody                                                              : TSmileyCircleDraw;
    DrawLeftEye                                                           : TSmileyCircleDraw;
    DrawRightEye                                                          : TSmileyCircleDraw;
  end;

  TSmoothyLineWS     = record
    Scale,LineOffset,AlphaWidth: Real;
    DrawProc                   : TVisProc;
  end;

  TSmoothyLineParams = packed record
    Color   : vpColor;
    Position: vpReal;
    Width   : vpReal;
    Vertical: vpBool;
  end;

const
  Smiley_DrawCircle: array [Boolean] of TSmileyCircleDraw = (@Smiley_DrawDefaultCircle,@Smiley_DrawDataCircle);

  NAngleIV     : TNAngleParams = (Radius: Real(50.0); Angles: Integer(6); PosX: Real(0.0); PosY: Real(0.0); Phi: Real(0.0); Visible: Boolean(true));
  TrigCircleIV : TTrigCircleParams = (Radius: Real(50.0); PosX: Real(0.0); PosY: Real(0.0); Phi: Real(0.0); Scale: Real(1.0); Data: (P1:0;P2:0;P3:0;P4:0); Visible: true);
  TrigRainIV   : TTrigRainParams = (RLength: Real(20.0); RDist: Real(0.0); Speed: Real(1.0); Phi: Real(0.0); Scale: Real(1.0); Data: (P1:0;P2:0;P3:0;P4:0); Visible: true);
  TrigPartIV   : TTrigPartParams = (C4: $FFFFFF00; XData: (P1:0;P2:0;P3:0;P4:0); YData: (P1:0;P2:0;P3:0;P4:0); XScale: 1.0; YScale: 1.0; XOffset: 0.0; YOffset: 0.0; Size: 4.0; Visible: true);
  ProLightIV   : TProLightParams = (LineColor: $FFFFFF00; BGColor: $77FFFF00; Rotation: 0.0; AutoRot: 3.0; Width: 42.0);
  SmoothyLineIV: TSmoothyLineParams = (Color: $FF00FF00; Position: 0.0; Width: 10.0; Vertical: true);
  SmileyIV     : TSmileyParams = (Size:50.0; LeftEye: 100.0; RightEye: 100.0; MouthAngle: 100.0; MouthOpen: 0.0; Color: $FFFFFF00; LeftEyeColor: $FFFFFF00; RightEyeColor: $FFFFFF00; Mouthcolor: $FFFFFF00; Data: (P1:0;P2:0;P3:0;P4:0); Scale: 1.0; LeftEyeData: (P1:0;P2:0;P3:0;P4:0); LeftEyeScale: 1.0; RightEyeData: (P1:0;P2:0;P3:0;P4:0); RightEyeScale: 1.0; MouthData: (P1:0;P2:0;P3:0;P4:0); MouthScale: 1.0; MouthOpenData: (P1:0;P2:0;P3:0;P4:0); MouthOpenScale: 1.0);

function HLCut(const Value,High: Real; const Low: Real = 0.0): Real;

implementation

function HLCut(const Value,High: Real; const Low: Real = 0.0): Real;
begin
  if Value<=High then begin
    if Value>=Low
      then Result:=Value
      else Result:=Low;
  end else Result:=High;
end;

procedure DrawNAngle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TNAngleParams absolute Params;
  //AWSRec : TNAngleWS absolute Workspace;
  ACenter : TPoint;
  I       : Integer;
  PhiStep : Real;
  PhiPos  : Real;
  StartPos: TPoint;
  NewPos2 : TPoint;
  OldPos2 : TPoint;
  ARadius : Real;
  ASize   : Real;
begin
  if not AParams.Visible then exit;
  if AParams.Angles<1 then exit;
  PhiStep:=(2*pi)/AParams.Angles;
  if Dest.Width<Dest.Height
    then ASize:=Dest.Width/200.0
    else ASize:=Dest.Height/200.0;
  ARadius:=AParams.Radius*ASize;
  //ACenter:=Point(bmp.Width div 2,bmp.Height div 2);
  ACenter:=Classes.Point(Round((Dest.Width/2)+((Dest.Width/200.0)*AParams.PosX)),Round((Dest.Height/2)+((Dest.Height/200.0)*AParams.PosY)));
  PhiPos:=(AParams.Phi/360.0)*2*pi;

  with StartPos do begin
    X:=Round((ARadius*sin(PhiPos))+ACenter.X);
    Y:=Round((ARadius*cos(PhiPos))+ACenter.Y);
  end;
  OldPos2:=StartPos;
  for I:=1 to AParams.Angles-1 do begin
    PhiPos+=PhiStep;
    with NewPos2 do begin
      X:=Round((ARadius*sin(PhiPos))+ACenter.X);
      Y:=Round((ARadius*cos(PhiPos))+ACenter.Y);
    end;
    Dest.Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,Visualisation.C1);
    OldPos2:=NewPos2;
  end;
  Dest.Line(OldPos2.X,OldPos2.Y,StartPos.X,StartPos.Y,Visualisation.C1);
end;

procedure DrawTrigCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

  function GetALineColor(const AValue: Real): vpColor;
  var
    c1,r,g,b: Integer;
  begin
    c1 := abs(Round(AValue*300));
    if c1>255 then c1:=255;
    c1:=255-c1;
    Result:=BetaBlend(Visualisation.C1,Visualisation.C2,c1);
  end;

var
  AParams   : TTrigCircleParams absolute Params;
  ACenter   : TPoint;
  I         : Integer;
  PhiStep   : Real;
  PhiPos    : Real;
  StartPos  : TPoint;
  NewPos2   : TPoint;
  OldPos2   : TPoint;
  ARadius   : Real;
  ASize     : Real;
  ACount    : Integer;
  AVal      : Real;

  function DoGetBufferItem(AIndex: Integer): Real; inline;
  begin
    Result:=PluginSystem.BufferManager.GetBufferItem(AParams.Data,AIndex)*AParams.Scale*ASize;
  end;

begin
  if not AParams.Visible then exit;
  with PluginSystem.BufferManager do begin
    ACount:=SizeOfBuffer(AParams.Data);
    if ACount<1 then exit;
  end;
  PhiStep:=(2*pi)/ACount;
  if (Dest.Width<Dest.Height) xor (AParams.Radius<0)
    then ASize:=Dest.Width/200.0
    else ASize:=Dest.Height/200.0;
  ARadius:=AParams.Radius*ASize;
  //ACenter:=Point(bmp.Width div 2,bmp.Height div 2);
  ACenter:=Classes.Point(Round((Dest.Width/2)+((Dest.Width/200.0)*AParams.PosX)),Round((Dest.Height/2)+((Dest.Height/200.0)*AParams.PosY)));
  PhiPos:=(AParams.Phi/360.0)*2*pi;

  with StartPos do begin
    AVal:=ARadius+DoGetBufferItem(0);
    X:=Round((AVal*sin(PhiPos))+ACenter.X);
    Y:=Round((AVal*cos(PhiPos))+ACenter.Y);
  end;
  OldPos2:=StartPos;
  for I:=1 to ACount-1 do begin
    PhiPos+=PhiStep;
    with NewPos2 do begin
      AVal:=ARadius+DoGetBufferItem(I);
      X:=Round((AVal*sin(PhiPos))+ACenter.X);
      Y:=Round((AVal*cos(PhiPos))+ACenter.Y);
    end;
    Dest.Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,Visualisation.C1);
    OldPos2:=NewPos2;
  end;
  Dest.Line(OldPos2.X,OldPos2.Y,StartPos.X,StartPos.Y,Visualisation.C1);
end;

procedure DrawTrigSmoothCircle(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

  function GetALineColor(const AValue: Real): vpColor;
  var
    c1,r,g,b: Integer;
  begin
    c1 := abs(Round(AValue*300));
    if c1>255 then c1:=255;
    c1:=255-c1;
    Result:=BetaBlend(Visualisation.C1,Visualisation.C2,c1);
  end;

var
  AParams   : TTrigCircleParams absolute Params;
  ACenter   : TPoint;
  I         : Integer;
  PhiStep   : Real;
  PhiPos    : Real;
  StartPos  : TPoint;
  NewPos2   : TPoint;
  OldPos2   : TPoint;
  ARadius   : Real;
  ASize     : Real;
  ACount    : Integer;
  AVal      : Real;
  AAdjustBuf: Real;

  function DoGetBufferItem(AIndex: Integer): Real; inline;
  begin
    Result:=(PluginSystem.BufferManager.GetBufferItem(AParams.Data,AIndex)+AAdjustBuf*AIndex)*AParams.Scale*ASize;
  end;

begin
  if not AParams.Visible then exit;
  with PluginSystem.BufferManager do begin
    ACount:=SizeOfBuffer(AParams.Data);
    if ACount<1 then exit;
    AAdjustBuf:=(GetBufferItem(AParams.Data,0)-GetBufferItem(AParams.Data,ACount-1))/ACount;
  end;
  PhiStep:=(2*pi)/ACount;
  if (Dest.Width<Dest.Height) xor (AParams.Radius<0)
    then ASize:=Dest.Width/200.0
    else ASize:=Dest.Height/200.0;
  ARadius:=AParams.Radius*ASize;
  //ACenter:=Point(bmp.Width div 2,bmp.Height div 2);
  ACenter:=Classes.Point(Round((Dest.Width/2)+((Dest.Width/200.0)*AParams.PosX)),Round((Dest.Height/2)+((Dest.Height/200.0)*AParams.PosY)));
  PhiPos:=(AParams.Phi/360.0)*2*pi;

  with StartPos do begin
    AVal:=ARadius+DoGetBufferItem(0);
    X:=Round((AVal*sin(PhiPos))+ACenter.X);
    Y:=Round((AVal*cos(PhiPos))+ACenter.Y);
  end;
  OldPos2:=StartPos;
  for I:=1 to ACount-1 do begin
    PhiPos+=PhiStep;
    with NewPos2 do begin
      AVal:=ARadius+DoGetBufferItem(I);
      X:=Round((AVal*sin(PhiPos))+ACenter.X);
      Y:=Round((AVal*cos(PhiPos))+ACenter.Y);
    end;
    Dest.Line(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,Visualisation.C1);
    OldPos2:=NewPos2;
  end;
  Dest.Line(OldPos2.X,OldPos2.Y,StartPos.X,StartPos.Y,Visualisation.C1);
end;

procedure DrawTrigRain(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
const
  _DoSteps     = 15;
  _DoColorSteps= 255/(_DoSteps-1);
var
  LVec2: TRealPoint;

  function GetALineColor(const AValue: Real): vpColor;
  var
    c1,r,g,b: Integer;
  begin
    c1 := abs(Round(AValue));
    if c1>255 then c1:=255;
    c1:=255-c1;
    Result:=BetaBlend(Visualisation.C1,Visualisation.C2,c1);
  end;

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
      Dest.Line(APos.X,APos.Y,APos2.X,APos2.Y,GetALineColor(I*_DoColorSteps));
      APos:=APos2;
    end;
  end;

var
  AParams       : TTrigRainParams absolute Params;
  AWSRec        : TRainWS absolute Workspace;
  ACenter       : TPoint;
  I,J           : Integer;
  //PhiStep       : Real;
  PhiPos        : Real;
  StartPos      : TRealPoint;
  AStep         : Real;
  //NewPos2       : TPoint;
  //OldPos2       : TPoint;
  //ARadius : Real;
  ASize         : Real;
  ASize2        : Real;
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
  if not AParams.Visible then exit;
  ACount:=PluginSystem.BufferManager.SizeOfBuffer(AParams.Data);
  if ACount<1 then exit;
  //PhiStep:=(2*pi)/ACount;
  if Dest.Width<Dest.Height
    then ASize2:=Dest.Width/200.0
    else ASize2:=Dest.Height/200.0;
  ASize:=sqrt(sqr(Dest.Width)+sqr(Dest.Height));
  //ARadius:=AParams.Radius*ASize;
  ACenter:=Classes.Point(Round(Dest.Width/2),Round(Dest.Height/2));
  //ACenter:=Point(Round((bmp.Width/2)+((bmp.Width/200.0)*AParams.PosX)),Round((bmp.Height/2)+((bmp.Height/200.0)*AParams.PosY)));
  PhiPos:=(AParams.Phi/360.0)*2*pi;
  ABPhi:=Arcsin(Dest.Height/ASize);

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
  {
  AWidth:=Abs(cos(PhiPos-ABPhi)*ASize);
  AHeight:=Abs(cos(ABPhi-PhiPos)*ASize);
  }
  ARainSize:=AParams.RLength+AParams.RDist;
  AStep:=100.0/(ACount{AParams.RDist}-1);
  AXStep:=AWidth/100.0;
  AYStep:=AHeight/100.0;

  LengthVector.X:=AParams.RLength*AYStep*sin(PhiPos);
  LengthVector.Y:=AParams.RLength*AYStep*cos(PhiPos);
  DistVector.X:=AParams.RDist*AYStep*sin(PhiPos);
  DistVector.Y:=AParams.RDist*AYStep*cos(PhiPos);
  LVec2.X:=LengthVector.X/_DoSteps;
  LVec2.Y:=LengthVector.Y/_DoSteps;

  MaxDraw:=Trunc(100.0/(AParams.RLength+AParams.RDist))+2;

  {with StartPos do begin
    AVal:=BufferManager.GetBufferItem(AParams.Data,0)*AYStep;
    X:=0;
    Y:=Round(AVal);
    //X:=Round((AVal*sin(PhiPos))+ACenter.X);
    //Y:=Round((AVal*cos(PhiPos))+ACenter.Y);
  end;}
  //OldPos2:=StartPos;
  for I:=0 to ACount-1 do begin
    //PhiPos+=PhiStep;
    with {NewPos2}StartPos do begin
      //AVal:=BufferManager.GetBufferItem(AParams.Data,I)*AParams.Scale;
      AVal:=(((Frac((PluginSystem.BufferManager.GetBufferItem(AParams.Data,I)*AParams.Scale)/(ARainSize))*ARainSize)-ARainSize+AWSRec.RainPos)*AYStep);

      X:=(I*AStep*AXStep)-(AWidth/2);
      Y:=AVal-(AHeight/2);
      AVR:=sqrt(sqr(X)+sqr(Y));
      AVP:=Arctan2(Y,X);
      X:=((AVR*cos(AVP-PhiPos))+(Dest.Width/2));
      Y:=((AVR*sin(AVP-PhiPos))+(Dest.Height/2));
      //X:=Round((AVal*sin(PhiPos))+ACenter.X);
      //Y:=Round((AVal*cos(PhiPos))+ACenter.Y);
    end;
    for J:=0 to MaxDraw-1 do begin
      DoDrawRainLine(StartPos.X,StartPos.Y);
      //bmp.LineFS(StartPos.X,StartPos.Y,StartPos.X+LengthVector.X,StartPos.Y+LengthVector.Y,Visualisation.C1);
      with StartPos do begin
        X+=LengthVector.X+DistVector.X;
        Y+=LengthVector.Y+DistVector.Y;
      end;
    end;
    //bmp.LineFS(OldPos2.X,OldPos2.Y,NewPos2.X,NewPos2.Y,Visualisation.C1);
    //OldPos2:=NewPos2;
  end;
  //bmp.LineFS(OldPos2.X,OldPos2.Y,StartPos.X,StartPos.Y,Visualisation.C1);
  AWSRec.RainPos+=AParams.Speed;
  if AWSRec.RainPos>=AParams.RLength+AParams.RDist then AWSRec.RainPos-=AParams.RLength+AParams.RDist;
end;

procedure InitTrigRain(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TRainWS absolute Workspace;
begin
  AWSRec.RainPos:=0.0;
end;

procedure DrawTrigPart(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams           : TTrigPartParams absolute Params;
  AMax,ABeta        : Cardinal;
  I                 : Integer;
  AVal,ACenter,AStep: TRealPoint;
  AColor            : vpColor;
  SizeOffset        : Real;
const
  BetaStep = 255.0/200.0;
begin
  with AParams do begin
    AMax:=PluginSystem.BufferManager.SizeOfBuffer(XData);
    if AMax>PluginSystem.BufferManager.SizeOfBuffer(YData)
      then AMax:=PluginSystem.BufferManager.SizeOfBuffer(YData);
    ACenter:=RealPoint(Dest.Width/2.0,Dest.Height/2.0);
    AStep:=RealPoint(Dest.Width/200.0,Dest.Height/200.0);
    SizeOffset:=Size/2.0;
    for I:=0 to AMax-1 do begin
      AVal:=RealPoint((PluginSystem.BufferManager.GetBufferItem(XData,I)*XScale)+XOffset,-((PluginSystem.BufferManager.GetBufferItem(YData,I)*YScale)+YOffset));
      ABeta:=Round(HLCut(AVal.X+100.0,200.0)*BetaStep);
      AColor:=BetaBlend(BetaBlend(C4,Visualisation.C3,ABeta),BetaBlend(Visualisation.C2,Visualisation.C1,ABeta),Round(HLCut(AVal.Y+100.0,200.0)*BetaStep));
      AVal.X:=(AVal.X*AStep.X)+ACenter.X;
      AVal.Y:=(AVal.Y*AStep.Y)+ACenter.Y;
      Dest.FillRect(Round(AVal.X-SizeOffset),Round(AVal.Y-SizeOffset),Round(AVal.X+SizeOffset),Round(AVal.Y+SizeOffset),AColor);
    end;
  end;
end;

procedure InitProLightning(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TProLightWS absolute Workspace;
begin
  with AWSRec do begin
    Rot:=0.0;
    Center:=RealPoint(Dest.Width/2.0,Dest.Height/2.0);
    if Dest.Width<Dest.Height
      then R:=Dest.Width
      else R:=Dest.Height;
  end;
end;

procedure DrawProLightning(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TProLightParams absolute Params;
  AWSRec    : TProLightWS absolute Workspace;
  ARot,ARot2: Real;
  I         : Integer;
const
  PiToDegree = 180.0/Pi;
  DegreeToPi = Pi/180.0;
begin
  if Random>0.75 then AWSRec.Rot+=PhiCut(Random*AParams.AutoRot*DegreeToPi);
  ARot:=AWSRec.Rot+(AParams.Rotation*DegreeToPi);
  ARot2:=ARot+(AParams.Width*DegreeToPi);
  //Draw
  Dest.Line(AWSRec.Center.X,AWSRec.Center.Y,AWSRec.Center.X+AWSRec.R*Sin(ARot),AWSRec.Center.Y+AWSRec.R*Cos(ARot),AParams.LineColor);
  Dest.Line(AWSRec.Center.X,AWSRec.Center.Y,AWSRec.Center.X+AWSRec.R*Sin(ARot2),AWSRec.Center.Y+AWSRec.R*Cos(ARot2),AParams.LineColor);
end;


procedure InitSmiley(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TSmileyParams absolute Params;
  AWSRec    : TSmileyWS absolute Workspace;
  //ASizeRel  : Real; //Faktor, mit dem die Größen in Prozent multipliziert werden müssen
  //AEyeOffset: Real;
{const
  _EyeBodyDist = 0.4;
  _EyeBodyDistI= 1.0-_EyeBodyDist;}
begin
  with AWSRec do begin
    Center:=RealPoint(Dest.Width/2,Dest.Height/2);
    if Dest.Width>=Dest.Height
      then BodyRel:=(Dest.Height/200.0) //2*100: 2 wg + oder -; 100 wg %
      else BodyRel:=(Dest.Width/200.0);

    ChangeSmileyBodySize(Dest,Source,Visualisation,Params,Workspace);

    DrawBody:=Smiley_DrawCircle[PluginSystem.BufferManager.Size[AParams.Data]>0];
    DrawLeftEye:=Smiley_DrawCircle[PluginSystem.BufferManager.Size[AParams.LeftEyeData]>0];
    DrawRightEye:=Smiley_DrawCircle[PluginSystem.BufferManager.Size[AParams.RightEyeData]>0];
  end;
end;

procedure FreeSmiley(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin

end;

procedure ChangeSmileyBodySize(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TSmileyParams absolute Params;
  AWSRec    : TSmileyWS absolute Workspace;
  AEyeOffset: Real;
const
  _EyeBodyDist = 0.4;
  _EyeBodyDistI= 1.0-_EyeBodyDist;
begin
  with AWSRec do begin
    BodySize:=BodyRel*AParams.Size;
    BodyScale:=BodyRel*AParams.Scale;

    EyeRel:=BodySize*_EyeBodyDist/100.0; //100 wg %
    {LeftEyeSize:=ASizeRel*AParams.LeftEye;
    LeftEyeScale:=ASizeRel*AParams.LeftEyeScale;
    RightEyeSize:=ASizeRel*AParams.RightEye;
    RightEyeScale:=ASizeRel*AParams.RightEyeScale;}

    AEyeOffset:=sqrt(sqr(BodySize*_EyeBodyDistI)/2);
    LeftEyePos:=RealPoint(Center.X-AEyeOffset,Center.Y-AEyeOffset);
    RightEyePos:=RealPoint(Center.X+AEyeOffset,Center.Y-AEyeOffset);
  end;
end;

procedure ChangeSmileyData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSmileyParams absolute Params;
  AWSRec : TSmileyWS absolute Workspace;
begin
  AWSRec.DrawBody:=Smiley_DrawCircle[PluginSystem.BufferManager.Size[AParams.Data]>0];
end;

procedure ChangeSmileyLeftEyeData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSmileyParams absolute Params;
  AWSRec : TSmileyWS absolute Workspace;
begin
  AWSRec.DrawLeftEye:=Smiley_DrawCircle[PluginSystem.BufferManager.Size[AParams.LeftEyeData]>0];
end;

procedure ChangeSmileyRightEyeData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSmileyParams absolute Params;
  AWSRec : TSmileyWS absolute Workspace;
begin
  AWSRec.DrawRightEye:=Smiley_DrawCircle[PluginSystem.BufferManager.Size[AParams.RightEyeData]>0];
end;

procedure ChangeSmileyMouthData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin

end;

procedure ChangeSmileyMouthOpenData(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin

end;

procedure Smiley_DrawDefaultCircle(Dest: IMCanvas; Pos: TRealPoint; R,Scale: Real; Color: vpColor; Buf: vpRealBuffer);
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
    Dest.Line(APos.X,APos.Y,APos2.X,APos2.Y,Color);
    APos:=APos2;
  end;
  Dest.Line(APos.X,APos.Y,Pos.X,Pos.Y+R,Color);
end;

procedure Smiley_DrawDataCircle(Dest: IMCanvas; Pos: TRealPoint; R,Scale: Real; Color: vpColor; Buf: vpRealBuffer);
var
  I,ASteps            : Integer;
  AIncrement,APhi,AR  : Real;
  APos,APos2          : TRealPoint;

  function GetRVal(Index: Integer): Real; inline;
  begin
    Result:=R+PluginSystem.BufferManager.Items[Buf,Index]*Scale;
  end;

begin
  ASteps:=PluginSystem.BufferManager.Size[Buf];
  AIncrement:=(2*Pi)/ASteps;

  APhi:=AIncrement;
  APos:=RealPoint(Pos.X,GetRVal(0)+Pos.Y);(*GetRVal(0)*Sin(APhi),GetRVal(0)*Cos(APhi)*)
  for I:=1 to ASteps-1 do begin
    AR:=GetRVal(I);
    APos2:=RealPoint(AR*Sin(APhi)+Pos.X,AR*Cos(APhi)+Pos.Y);
    APhi+=AIncrement;
    Dest.Line(APos.X,APos.Y,APos2.X,APos2.Y,Color);
    APos:=APos2;
  end;
  Dest.Line(APos.X,APos.Y,Pos.X,GetRVal(0)+Pos.Y,Color);
end;

procedure DrawSmiley(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSmileyParams absolute Params;
  AWSRec : TSmileyWS absolute Workspace;
begin
  with AWSRec do begin
    DrawBody(Dest,Center,BodySize,BodyScale,AParams.Color,AParams.Data);
    DrawLeftEye(Dest,LeftEyePos,AParams.LeftEye*EyeRel,AParams.LeftEyeScale*EyeRel,AParams.LeftEyeColor,AParams.LeftEyeData);
    DrawRightEye(Dest,RightEyePos,AParams.RightEye*EyeRel,AParams.RightEyeScale*EyeRel,AParams.RightEyeColor,AParams.RightEyeData);
  end;
end;

procedure ChangeSmoothyLineWidth(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSmoothyLineParams absolute Params;
  AWSRec : TSmoothyLineWS absolute Workspace;
begin
  with AWSRec do LineOffset:=Round(AParams.Width*Scale);
end;

procedure ChangeSmoothyLineOrientation(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSmoothyLineParams absolute Params;
  AWSRec : TSmoothyLineWS absolute Workspace;
begin
  if AParams.Vertical then with AWSRec do begin
    DrawProc:=@DrawSmoothyLine_Vertical;
    Scale:=Dest.Width/200.0;
  end else with AWSRec do begin
    DrawProc:=@DrawSmoothyLine_Horizontal;
    Scale:=Dest.Height/200.0;
  end;
  ChangeSmoothyLineWidth(Dest,Source,Visualisation,Params,Workspace);
end;

procedure DrawSmoothyLine(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSmoothyLineParams absolute Params;
  AWSRec : TSmoothyLineWS absolute Workspace;
begin
  AWSRec.DrawProc(Dest,Source,Visualisation,Params,Workspace);
end;

procedure DrawSmoothyLine_Vertical(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams                                   : TSmoothyLineParams absolute Params;
  AWSRec                                    : TSmoothyLineWS absolute Workspace;
  I                                         : Integer;
  ASmoothColor                              : vpColor;
  AStartAlpha                               : Byte;
  AAlphaWidth,APos,APosL,APosR,ANPosL,ANPosR: Real;
begin
  AStartAlpha:=(AParams.Color and $FF000000) shr 24;
  //Wenn Alpha 0 ist würde man sowieso nichts sehen...
  //Außerdem verursacht es einen Absturz bei der Division...
  if AStartAlpha=0 then exit;
  APos:=AWSRec.Scale*(AParams.Position+100.0);
  ASmoothColor:=AParams.Color and $00FFFFFF;
  AAlphaWidth:=AWSRec.LineOffset/AStartAlpha;
  APosL:=APos;
  APosR:=APos;
  for I:=AStartAlpha downto 1 do begin
    ANPosL:=APosL-AAlphaWidth;
    ANPosR:=APosR+AAlphaWidth;
    Dest.FillRect(ANPosL,0,APosL,Dest.Height,ASmoothColor or (I shl 24));
    Dest.FillRect(APosR,0,ANPosR,Dest.Height,ASmoothColor or (I shl 24));
    APosL:=ANPosL;
    APosR:=ANPosR;
  end;
end;

procedure DrawSmoothyLine_Horizontal(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams                                   : TSmoothyLineParams absolute Params;
  AWSRec                                    : TSmoothyLineWS absolute Workspace;
  I                                         : Integer;
  ASmoothColor                              : vpColor;
  AStartAlpha                               : Byte;
  AAlphaWidth,APos,APosT,APosB,ANPosT,ANPosB: Real;
begin
  AStartAlpha:=(AParams.Color and $FF000000) shr 24;
  //Wenn Alpha 0 ist würde man sowieso nichts sehen...
  //Außerdem verursacht es einen Absturz bei der Division...
  if AStartAlpha=0 then exit;
  APos:=AWSRec.Scale*(AParams.Position+100.0);
  ASmoothColor:=AParams.Color and $00FFFFFF;
  AAlphaWidth:=AWSRec.LineOffset/AStartAlpha;
  APosT:=APos;
  APosB:=APos;
  for I:=AStartAlpha downto 1 do begin
    ANPosT:=APosT-AAlphaWidth;
    ANPosB:=APosB+AAlphaWidth;
    Dest.FillRect(0,ANPosT,Dest.Width,APosT,ASmoothColor or (I shl 24));
    Dest.FillRect(0,APosB,Dest.Width,ANPosB,ASmoothColor or (I shl 24));
    APosT:=ANPosT;
    APosB:=ANPosB;
  end;
end;

end.

