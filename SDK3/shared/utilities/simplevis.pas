unit SimpleVis;

(*
  helper unit to make input and output access easier. The SDK works fine
  completely without this.
*)

{$mode objfpc}

interface

uses
  Classes, SysUtils, StdParamTypes, VisEventImpl, VisType2, MStrings;

type
  TVisualisationParams               = class helper for TVisualisationEvents
  private
    //input getters
    function GetCallInput(AName: IString): IPCall;
    function GetIntegerInput(AName: IString): IPInteger;
    function GetFloatInput(AName: IString): IPFloat;
    function GetColorInput(AName: IString): IPColor;
    function GetBooleanInput(AName: IString): IPBoolean;
    function GetBufferInput(AName: IString): IPBuffer;
    function GetStringInput(AName: IString): IPString;
    function GetPresetInput(AName: IString): IPPreset;
    function GetPointerInput(AName: IString): IPPointer;
  public
    //typed inputs
    property CallInputs[AName: IString]: IPCall read GetCallInput;
    property IntegerInputs[AName: IString]: IPInteger read GetIntegerInput;
    property FloatInputs[AName: IString]: IPFloat read GetFloatInput;
    property ColorInputs[AName: IString]: IPColor read GetColorInput;
    property BooleanInputs[AName: IString]: IPBoolean read GetBooleanInput;
    property BufferInputs[AName: IString]: IPBuffer read GetBufferInput;
    property StringInputs[AName: IString]: IPString read GetStringInput;
    property PresetInputs[AName: IString]: IPPreset read GetPresetInput;
    property PointerInputs[AName: IString]: IPPointer read GetPointerInput;
  end;

implementation

{%REGION TVisualisationParams}

function TVisualisationParams.GetCallInput(AName: IString): IPCall;
begin
  Result:=IPCall(Prototype.Inputs[ParamID(AName, vCall)]);
end;

function TVisualisationParams.GetIntegerInput(AName: IString): IPInteger;
begin
  Result:=IPInteger(Prototype.Inputs[ParamID(AName, vInteger)]);
end;

function TVisualisationParams.GetFloatInput(AName: IString): IPFloat;
begin
  Result:=IPFloat(Prototype.Inputs[ParamID(AName, vFloat)]);
end;

function TVisualisationParams.GetColorInput(AName: IString): IPColor;
begin
  Result:=IPColor(Prototype.Inputs[ParamID(AName, vColor)]);
end;

function TVisualisationParams.GetBooleanInput(AName: IString): IPBoolean;
begin
  Result:=IPBoolean(Prototype.Inputs[ParamID(AName, vBoolean)]);
end;

function TVisualisationParams.GetBufferInput(AName: IString): IPBuffer;
begin
  Result:=IPBuffer(Prototype.Inputs[ParamID(AName, vBuffer)]);
end;

function TVisualisationParams.GetStringInput(AName: IString): IPString;
begin
  Result:=IPString(Prototype.Inputs[ParamID(AName, vString)]);
end;

function TVisualisationParams.GetPresetInput(AName: IString): IPPreset;
begin
  Result:=IPPreset(Prototype.Inputs[ParamID(AName, vPreset)]);
end;

function TVisualisationParams.GetPointerInput(AName: IString): IPPointer;
begin
  Result:=IPPointer(Prototype.Inputs[ParamID(AName, vPointer)]);
end;

{%ENDREGION}

end.

