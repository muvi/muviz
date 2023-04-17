unit DefaultTriggers; 

{$mode objfpc}{$H+}

interface

uses
  VisType, SpectrumData, GraphX32, AdvFunc, Math, VPBuffers;

type
  TFFTParams           = packed record
    Index: vpInt;
    Scale: vpReal;
  end;

  TRoundParams         = packed record
    Input,Scale: vpReal;
  end;

  TIntParams           = vpReal;

  TCalcParams          = packed record
    Input1,Input2: vpReal;
  end;

  TSwitchParams        = vpInt;
  TCounterParams       = vpInt;

  TCounter2Params      = packed record
    Max,Min,Start,Step: vpReal;
  end;

  TCounterWS           = vpInt;

  TLimitCallParams     = packed record
    Input,Limit: vpReal;
  end;

  TLimitStopWS         = vpBool;

  TLimitStopCallParams = packed record
    Input,Limit: vpReal;
  end;

  TPeakTrigParams      = vpReal;
  TColorizeParams      = vpInt;
  TBackCounterParams   = packed record
    Min,Max,Step: vpInt;
  end;

  TBackCounterWS       = packed record
    Pos      : vpInt;
    Direction: SmallInt;
  end;

  TIntToRealParams     = vpInt;
  TMultiIntParams      = vpReal;
  TSqrtParams          = vpReal;
  TBoolCallParams      = vpBool;
  TBoolCallWS          = vpBool;
  TCounter2WS          = vpReal;
  TNotTrigParams       = vpBool;
  TChangeCallParams    = vpReal;
  TChangeCallWS        = vpReal;
  TColorToRGBParams    = vpColor;
  TColorToHSVParams    = vpColor;

  TRGBToColorParams    = packed record
    R,G,B,A: vpInt;
  end;

  THSVToColorParams    = packed record
    H    : vpReal;
    S,V,A: vpInt;
  end;

  TStepBackCounterWS   = packed record
    Pos,Direction: vpReal;
  end;

const
  FFTIV            : TFFTParams        = (Index: 0; Scale: 20.0);
  RoundIV          : TRoundParams      = (Input: 0.0; Scale: 1.0);
  IntIV            : TIntParams        = 0.0;
  CalcIV           : TCalcParams       = (Input1: 0.0; Input2: 0.0);
  CalcIV2          : TCalcParams       = (Input1: 0.0; Input2: 1.0);
  SwitchIV         : TSwitchParams     = 0;
  CounterIV        : TCounterParams    = 4;
  Counter2IV       : TCounter2Params   = (Max: Real(100.0); Min: Real(0.0); Start: Real(0.0); Step: Real(1.0));
  LimitCallIV      : TLimitCallParams  = (Input: 0.0; Limit: 0.3);
  LimitStopCallIV  : TLimitCallParams  = (Input: 0.0; Limit: 0.3);
  PeakTrigIV       : TPeakTrigParams   = 20.0;
  ColorizeIV       : TColorizeParams   = 127;
  BackCounterIV    : TBackCounterParams= (Min: 0; Max: 3; Step: 1);
  IntToRealIV      : TIntToRealParams  = 0;
  MultiIntIV       : TMultiIntParams   = 0;
  SqrtIV           : TSqrtParams       = 1.0;
  BoolCallIV       : TBoolCallParams   = false;
  NotTrigIV        : TNotTrigParams    = false;
  ChangeCallIV     : TChangeCallParams = 0.0;
  ColorToRGBIV     : TColorToRGBParams = $FFFFFF00;
  ColorToHSVIV     : TColorToRGBParams = $FFFFFF00;
  RGBToColorIV     : TRGBToColorParams = (R: $FF; G: $00; B: $00; A: $FF);
  HSVToColorIV     : THSVToColorParams = (H: 180.0; S: $FF; V: $FF; A: $FF);
  IntegerParamIV   : vpInt = 0;
  RealParamIV      : vpReal = 0.0;
  StringParamIV    : vpString = '';
  ColorParamIV     : vpColor = $00000000;
  BooleanParamIV   : vpBool = false;

procedure SetCallStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure SetIntegerStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure SetRealStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure SetStringStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure SetColorStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure SetBooleanStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure SetRealBufferStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure InitCounter(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitCounter2(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitStepBackCounter(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure StepBackCountChangeStep(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitLimitStopCaller(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitBackCounter(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitBoolCaller(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure InitChangeCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeCallChange(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure TrigFFT(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigRound(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigInt(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigAdd(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigSwitch4(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigSwitch10(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigMulti4(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigCount(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigCount2(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigStepBackCount(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigLimitCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigSetColor(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigLimitStopCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigPeak(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigColorize(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBackCount(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigMultiply(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigDivide(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigMod(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigIntToReal(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigMultiInt(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigSqrt(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigBoolCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigNot(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

procedure ChangeColorToRGB(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeRGBToColor(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeColorToHSV(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure ChangeHSVToColor(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

const
  EMPTYOUTPUT: Byte = 0;

implementation

procedure TrigFFT(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TFFTParams absolute Params;
  AFFTVal: vpReal;
begin
  AFFTVal:=Source.Levels[AParams.Index,0]*AParams.Scale;
  Visualisation.SetOutput(0,AFFTVal);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AFFTVal);
  //SetVisOutput2(Visualisation,AFFTVal,0);

  //PReal(Visualisation.VisOutputs[0])^:=Real(Source.Levels[AParams.Index]*AParams.Scale);
end;

procedure TrigRound(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRoundParams absolute Params;
  ARndVal: vpInt;
begin
  ARndVal:=Round(AParams.Input*AParams.Scale);
  Visualisation.SetOutput(0,ARndVal);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,ARndVal);
  //SetVisOutput2(Visualisation,ARndVal,0);
  //PInteger(Visualisation.VisOutputs[0])^:=Integer(Round(AParams.Input*AParams.Scale));
end;

procedure TrigInt(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRoundParams absolute Params;
  AResult: vpInt;
begin
  AResult:=Trunc(AParams.Input);
  Visualisation.SetOutput(0,AResult);
end;

procedure TrigAdd(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TCalcParams absolute Params;
  AAddVal: vpReal;
begin
  AAddVal:=AParams.Input1+AParams.Input2;
  Visualisation.SetOutput(0,AAddVal);
end;

procedure TrigSwitch4(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSwitchParams absolute Params;
begin
  if AParams<=3 then Visualisation.SetOutput(AParams,EMPTYOUTPUT);
end;

procedure TrigSwitch10(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSwitchParams absolute Params;
  AOver  : vpInt;
begin
  if AParams>=0 then begin
    if AParams<10
      then Visualisation.SetOutput(AParams,EMPTYOUTPUT)
      else begin
        AOver:=AParams-10;
        Visualisation.SetOutput(11,AOver);
        Visualisation.SetOutput(10,EMPTYOUTPUT);
      end;
  end;
end;

procedure TrigMulti4(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  Visualisation.SetOutput(0,EMPTYOUTPUT);
  Visualisation.SetOutput(1,EMPTYOUTPUT);
  Visualisation.SetOutput(2,EMPTYOUTPUT);
  Visualisation.SetOutput(3,EMPTYOUTPUT);
end;

procedure InitCounter(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TCounterWS absolute Workspace;
begin
  AWSRec:=0;
end;

procedure InitCounter2(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TCounter2WS absolute Workspace;
  AParams: TCounter2Params absolute Params;
begin
  AWSRec:=AParams.Start;
end;

procedure InitStepBackCounter(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TStepBackCounterWS absolute Workspace;
  AParams: TCounter2Params absolute Params;
begin
  with AWSRec do begin
    Pos:=AParams.Start;
    Direction:=AParams.Step;
  end;
end;

procedure StepBackCountChangeStep(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec : TStepBackCounterWS absolute Workspace;
  AParams: TCounter2Params absolute Params;
begin
  AWSRec.Direction:=Sign(AWSRec.Direction)*AParams.Step;
end;

procedure TrigCount(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TCounterParams absolute Params;
  AWSRec : TCounterWS absolute Workspace;
begin
  AWSRec:=(AWSRec+1) mod AParams;
  Visualisation.SetOutput(0,AWSRec);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AWSRec);
end;

procedure TrigCount2(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TCounter2Params absolute Params;
  AWSRec : TCounter2WS absolute Workspace;
begin
  AWSRec+=AParams.Step;
  if AWSRec>=AParams.Max then AWSRec:=AParams.Min+(AWSRec-AParams.Max);
  if AWSRec<=AParams.Min then AWSRec:=AParams.Max-(AParams.Min-AWSRec);
  Visualisation.SetOutput(0,AWSRec);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AWSRec);
end;

procedure TrigStepBackCount(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TCounter2Params absolute Params;
  AWSRec : TStepBackCounterWS absolute Workspace;
begin
  with AWSRec do begin
    Pos+=Direction;
    if Pos>=AParams.Max then begin
      Pos:=AParams.Max;//2*AParams.Max-Pos;
      Direction:=-Direction;
    end;
    if Pos<=AParams.Min then begin
      Pos:=AParams.Min;//2*AParams.Min-Pos;
      Direction:=-Direction;
    end;
    Visualisation.SetOutput(0,Pos);
  end;
end;

procedure TrigLimitCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TLimitCallParams absolute Params;
begin
  if AParams.Input>AParams.Limit then Visualisation.SetOutput(0,EMPTYOUTPUT);//with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,0);
  //then TriggerCall2(bmp,Source,Visualisation.VisOutputs[0]);
end;

procedure TrigSetColor(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  Visualisation.SetOutput(0,Visualisation.C1);
  Visualisation.SetOutput(1,Visualisation.C2);
  Visualisation.SetOutput(2,Visualisation.C3);

  {with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,Visualisation.C1);
  with Visualisation.VisOutputs[1] do DoSet(bmp,Source,Data,Visualisation.C2);
  with Visualisation.VisOutputs[2] do DoSet(bmp,Source,Data,Visualisation.C3);}

  {SetVisOutput2(Visualisation,Visualisation.C1,0);
  SetVisOutput2(Visualisation,Visualisation.C2,1);
  SetVisOutput2(Visualisation,Visualisation.C3,2);}
end;

procedure InitLimitStopCaller(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TLimitStopWS absolute Workspace;
begin
  AWSRec:=false;
end;

procedure TrigLimitStopCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams  : TLimitStopCallParams absolute Params;
  AWSRec   : TLimitStopWS absolute Workspace;
  NewIsCall: vpBool;
begin
  NewIsCall:=(AParams.Input>AParams.Limit);
  if NewIsCall and (not AWSRec) then Visualisation.SetOutput(0,EMPTYOUTPUT);//with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,0);
  // then TriggerCall2(bmp,Source,Visualisation.VisOutputs[0]);
  AWSRec:=NewIsCall;
end;

procedure TrigPeak(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TPeakTrigParams absolute Params;
  APeakVal: vpReal;
begin
  APeakVal:=Source.Peak[0]*AParams;
  Visualisation.SetOutput(0,APeakVal);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,APeakVal);
  //SetVisOutput2(Visualisation,APeakVal,0);
end;

procedure TrigColorize(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams2: TColorizeParams absolute Params;
  AParams : TColorizeParams;
  AClrVal : vpColor;
begin
  AParams:=AParams2;
  if AParams>255
    then AParams:=255
    else if AParams<0 then AParams:=0;
  AClrVal:=BetaBlend(Visualisation.C1,Visualisation.C2,AParams);
  Visualisation.SetOutput(0,AClrVal);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AClrVal);
  //SetVisOutput2(Visualisation,AClrVal,0);
end;

procedure InitBackCounter(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TBackCounterWS absolute Workspace;
begin
  AWSRec.Direction:=1;
  AWSRec.Pos:=0;
end;

procedure TrigBackCount(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams  : TBackCounterParams absolute Params;
  AWSRec   : TBackCounterWS absolute Workspace;
begin
  AWSRec.Pos+=AParams.Step*AWSRec.Direction;
  if (AWSRec.Pos>AParams.Max) or (AWSRec.Pos<AParams.Min) then begin
    AWSRec.Direction:=-AWSRec.Direction;
    AWSRec.Pos+=AParams.Step*AWSRec.Direction;
  end;
  Visualisation.SetOutput(0,AWSRec.Pos);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AWSRec.Pos);
  //SetVisOutput2(Visualisation,AWSRec.Pos,0);
end;

procedure TrigMultiply(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TCalcParams absolute Params;
  ACalcVal: vpReal;
begin
  ACalcVal:=AParams.Input1*AParams.Input2;
  Visualisation.SetOutput(0,ACalcVal);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,ACalcVal);
end;

procedure TrigDivide(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TCalcParams absolute Params;
  ACalcVal: vpReal;
begin
  ACalcVal:=AParams.Input1/AParams.Input2;
  Visualisation.SetOutput(0,ACalcVal);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,ACalcVal);
end;

procedure TrigMod(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TCalcParams absolute Params;
  ACalcVal: vpReal;
begin
  ACalcVal:=RealMod(AParams.Input1,AParams.Input2);
  Visualisation.SetOutput(0,ACalcVal);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,ACalcVal);
end;

procedure TrigIntToReal(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TIntToRealParams absolute Params;
  ARealVal: vpReal;
begin
  ARealVal:=AParams;
  Visualisation.SetOutput(0,ARealVal);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,ARealVal);
  //SetVisOutput2(Visualisation,ARealVal,0);
end;

procedure TrigMultiInt(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams : TMultiIntParams absolute Params;
begin
  Visualisation.SetOutput(0,AParams);
  Visualisation.SetOutput(1,AParams);
  Visualisation.SetOutput(2,AParams);
  Visualisation.SetOutput(3,AParams);
  {with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AParams);
  with Visualisation.VisOutputs[1] do DoSet(bmp,Source,Data,AParams);
  with Visualisation.VisOutputs[2] do DoSet(bmp,Source,Data,AParams);
  with Visualisation.VisOutputs[3] do DoSet(bmp,Source,Data,AParams);}

  {SetVisOutput2(Visualisation,AParams,0);
  SetVisOutput2(Visualisation,AParams,1);
  SetVisOutput2(Visualisation,AParams,2);
  SetVisOutput2(Visualisation,AParams,3);}
end;

procedure TrigSqrt(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TSqrtParams absolute Params;
  AResult: vpReal;
begin
  AResult:=sqrt(AParams);
  Visualisation.SetOutput(0,AResult);
  //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,AResult);
  //SetVisOutput2(Visualisation,AResult,0);
end;

procedure InitBoolCaller(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AWSRec: TBoolCallWS absolute Workspace;
begin
  AWSRec:=false;
end;

procedure TrigBoolCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TBoolCallParams absolute Params;
  AWSRec : TBoolCallWS absolute Workspace;
begin
  if AParams and (not AWSRec) then Visualisation.SetOutput(0,EMPTYOUTPUT); //with Visualisation.VisOutputs[0] do DoSet(bmp,Source,Data,0);
  AWSRec:=AParams;
end;

procedure TrigNot(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TNotTrigParams absolute Params;
  AVal   : vpBool;
begin
  AVal:=not AParams;
  Visualisation.SetOutput(0,AVal);
end;

procedure InitChangeCall(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TChangeCallParams absolute Params;
  AWorkspace: TChangeCallWS absolute Workspace;
begin
  AWorkspace:=AParams;
end;

procedure ChangeCallChange(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams   : TChangeCallParams absolute Params;
  AWorkspace: TChangeCallWS absolute Workspace;
begin
  if AParams<>AWorkspace then begin
    AWorkspace:=AParams;
    Visualisation.SetOutput(0,EMPTYOUTPUT);
  end;
end;

procedure ChangeColorToRGB(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TColorToRGBParams absolute Params;
  AVal   : vpInt;
begin
  //R
  AVal:=(AParams shr 16) and $FF;
  Visualisation.SetOutput(0,AVal);
  //G
  AVal:=(AParams shr 8) and $FF;
  Visualisation.SetOutput(1,AVal);
  //B
  AVal:=AParams and $FF;
  Visualisation.SetOutput(2,AVal);
  //A
  AVal:=(AParams shr 24) and $FF;
  Visualisation.SetOutput(3,AVal);
end;

procedure ChangeRGBToColor(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRGBToColorParams absolute Params;
  AVal   : vpColor;
begin
  AVal:=IntForceMinMax2(AParams.B,0,$FF) or (IntForceMinMax2(AParams.G,0,$FF) shl 8) or (IntForceMinMax2(AParams.R,0,$FF) shl 16) or (IntForceMinMax2(AParams.A,0,$FF) shl 24);
  Visualisation.SetOutput(0,AVal);
end;

procedure ChangeColorToHSV(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TColorToHSVParams absolute Params;
  AHSV   : TAHSV;
  AHue   : vpReal;
  AVal   : vpInt;
begin
  AHSV:=ARGBToAHSV(AParams);
  //H
  AHue:=AHSV.H*DegreeFac;
  Visualisation.SetOutput(0,AHue);
  //S
  AVal:=AHSV.S;
  Visualisation.SetOutput(1,AVal);
  //V
  AVal:=AHSV.V;
  Visualisation.SetOutput(2,AVal);
  //A
  AVal:=AHSV.A;
  Visualisation.SetOutput(3,AVal);
end;

procedure ChangeHSVToColor(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: THSVToColorParams absolute Params;
  AVal   : vpColor;
  AHSV   : TAHSV;
begin
  AHSV.A:=AParams.A;
  AHSV.H:=RealMod(AParams.H/DegreeFac,2*pi);
  if AHSV.H<0 then AHSV.H:=2*pi-AHSV.H;
  AHSV.S:=AParams.S;
  AHSV.V:=AParams.V;
  AVal:=AHSVToARGB(AHSV);
  Visualisation.SetOutput(0,AVal);
end;

procedure SetCallStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
begin
  Visualisation.SetOutput(0,EMPTYOUTPUT);
end;

procedure SetIntegerStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: vpInt absolute Params;
begin
  Visualisation.SetOutput(0,AParams);
end;

procedure SetRealStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: vpReal absolute Params;
begin
  Visualisation.SetOutput(0,AParams);
end;

procedure SetStringStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: vpString absolute Params;
begin
  Visualisation.SetOutput(0,AParams);
end;

procedure SetColorStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: vpColor absolute Params;
begin
  Visualisation.SetOutput(0,AParams);
end;

procedure SetBooleanStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: vpBool absolute Params;
begin
  Visualisation.SetOutput(0,AParams);
end;

procedure SetRealBufferStorage(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: vpRealBuffer absolute Params;
begin
  Visualisation.SetOutput(0,AParams);
end;

end.

