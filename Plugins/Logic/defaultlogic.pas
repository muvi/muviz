unit DefaultLogic;

{$mode objfpc}{$H+}

interface

uses
  VisType, SpectrumData, GraphX32, AdvFunc, Math;

type
  TConditionParams  = vpBool;
  TRealCompareParams= packed record
    Val1,Val2: vpReal;
  end;
  TForParams        = packed record
    First,Last: vpInt;
  end;
  TForStepParams    = packed record
    First,Last,Step: vpInt;
  end;

const
  ConditionIV     : TConditionParams = false;
  NotConditionIV  : TConditionParams = true;
  RealCompareIV   : TRealCompareParams = (Val1: 0.0; Val2: 0.0);
  ForIV           : TForParams = (First: 0; Last: 10);
  ForStepIV       : TForStepParams = (First: 0; Last: 10; Step: 1);

procedure TrigIf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigIfElse(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigWhile(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigRepeat(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigForStep1(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigForStep_1(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigFor(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigRealEqual(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigRealNotEqual(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigRealLower(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigRealHigher(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigRealLowerOrEqual(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
procedure TrigRealHigherOrEqual(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;

const
  EMPTYOUTPUT: Byte = 0;

implementation

procedure TrigIf(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TConditionParams absolute Params;
begin
  if AParams
    then Visualisation.SetOutput(0,EMPTYOUTPUT);
end;

procedure TrigIfElse(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TConditionParams absolute Params;
begin
  if AParams
    then Visualisation.SetOutput(0,EMPTYOUTPUT)
    else Visualisation.SetOutput(1,EMPTYOUTPUT);
end;

procedure TrigWhile(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TConditionParams absolute Params;
begin
  while AParams
    do Visualisation.SetOutput(0,EMPTYOUTPUT);
end;

procedure TrigRepeat(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TConditionParams absolute Params;
begin
  repeat
    Visualisation.SetOutput(0,EMPTYOUTPUT);
  until AParams;
end;

procedure TrigForStep1(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TForParams absolute Params;
  I      : vpInt;
begin
  for I:=AParams.First to AParams.Last do begin
    Visualisation.SetOutput(1,I);
    Visualisation.SetOutput(0,EMPTYOUTPUT);
  end;
end;

procedure TrigForStep_1(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TForParams absolute Params;
  I      : vpInt;
begin
  for I:=AParams.First downto AParams.Last do begin
    Visualisation.SetOutput(1,I);
    Visualisation.SetOutput(0,EMPTYOUTPUT);
  end;
end;

procedure TrigFor(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TForStepParams absolute Params;
  J      : vpInt;
  AMax,I : Integer;
begin
  AMax:=((AParams.Last-AParams.First) div AParams.Step)+AParams.First;
  for I:=AParams.First to AMax do begin
    J:=I*AParams.Step;
    Visualisation.SetOutput(1,J);
    Visualisation.SetOutput(0,EMPTYOUTPUT);
  end;
end;

procedure TrigRealEqual(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRealCompareParams absolute Params;
  AResult: vpBool;
begin
  AResult:=IsZero(AParams.Val1-AParams.Val2);
  Visualisation.SetOutput(0,AResult);
end;

procedure TrigRealNotEqual(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRealCompareParams absolute Params;
  AResult: vpBool;
begin
  AResult:=(AParams.Val1<>AParams.Val2);
  Visualisation.SetOutput(0,AResult);
end;

procedure TrigRealLower(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRealCompareParams absolute Params;
  AResult: vpBool;
begin
  AResult:=(AParams.Val1<AParams.Val2);
  Visualisation.SetOutput(0,AResult);
end;

procedure TrigRealHigher(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRealCompareParams absolute Params;
  AResult: vpBool;
begin
  AResult:=(AParams.Val1>AParams.Val2);
  Visualisation.SetOutput(0,AResult);
end;

procedure TrigRealLowerOrEqual(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRealCompareParams absolute Params;
  AResult: vpBool;
begin
  AResult:=(AParams.Val1<=AParams.Val2);
  Visualisation.SetOutput(0,AResult);
end;

procedure TrigRealHigherOrEqual(Dest: IMCanvas; Source: ISpectrumData; Visualisation: IVisualisation; const Params; var Workspace); stdcall;
var
  AParams: TRealCompareParams absolute Params;
  AResult: vpBool;
begin
  AResult:=(AParams.Val1>=AParams.Val2);
  Visualisation.SetOutput(0,AResult);
end;

end.

