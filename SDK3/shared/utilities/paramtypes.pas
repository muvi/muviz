unit ParamTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamType2, VisType2, StdParamTypes, AdvParamType,
  PluginType;

function CreateCallParam(APrototype: IPParamPrototype): IPCall;
function CreateIntegerParam(APrototype: IPParamPrototype): IPInteger;
function CreateIntegerParam(AMin, AMax: TVInteger; APrototype: IPParamPrototype): IPInteger;
function CreateFloatParam(APrototype: IPParamPrototype): IPFloat;
function CreateFloatParam(AMin, AMax, ANan: TVFloat; APrototype: IPParamPrototype): IPFloat;
function CreateColorParam(APrototype: IPParamPrototype): IPColor;
function CreateBooleanParam(APrototype: IPParamPrototype): IPBoolean;
function CreateBufferParam(APrototype: IPParamPrototype): IPBuffer;
function CreateStringParam(APrototype: IPParamPrototype): IPString;
function CreatePresetParam(APrototype: IPParamPrototype): IPPreset;
function CreatePresetParam(APrototype: IPParamPrototype; AEnvironment: IPVisualisationEnvironment): IPPreset;
function CreatePointerParam(APrototype: IPParamPrototype): IPPointer;

implementation

function DoCreateStdParamSettings(APrototype: IPParamPrototype; AType: TPParamType): IPParamSettings; inline;
begin
  Assert(AType = APrototype.ID.&Type);
  Result:=ParamTypeUtil[AType].CreateValueStorage(APrototype);
end;

function DoCreateStdParam(APrototype: IPParamPrototype; AType: TPParamType): IPParam; inline;
var
  AParamSettings: IPParamSettings;
begin
  AParamSettings:=DoCreateStdParamSettings(APrototype, AType);
  Result:=AParamSettings.Param;
end;

function CreateCallParam(APrototype: IPParamPrototype): IPCall;
begin
  Result:=IPCall(DoCreateStdParam(APrototype, vCall));
end;

function CreateIntegerParam(APrototype: IPParamPrototype): IPInteger;
begin
  Result:=IPInteger(DoCreateStdParam(APrototype, vInteger));
end;

function CreateIntegerParam(AMin, AMax: TVInteger; APrototype: IPParamPrototype): IPInteger;
var
  AParamSettings: IPParamSettings;
begin
  AParamSettings:=DoCreateStdParamSettings(APrototype, vInteger);
  Result:=IPInteger(AParamSettings.Param);
  IPIntegerSettings(AParamSettings).SetBounds(AMin, AMax);
end;

function CreateFloatParam(APrototype: IPParamPrototype): IPFloat;
begin
  Result:=IPFloat(DoCreateStdParam(APrototype, vFloat));
end;

function CreateFloatParam(AMin, AMax, ANan: TVFloat; APrototype: IPParamPrototype): IPFloat;
var
  AParamSettings: IPParamSettings;
begin
  AParamSettings:=DoCreateStdParamSettings(APrototype, vFloat);
  Result:=IPFloat(AParamSettings.Param);
  IPFloatSettings(AParamSettings).SetBounds(AMin, AMax, ANan);
end;

function CreateColorParam(APrototype: IPParamPrototype): IPColor;
begin
  Result:=IPColor(DoCreateStdParam(APrototype, vColor));
end;

function CreateBooleanParam(APrototype: IPParamPrototype): IPBoolean;
begin
  Result:=IPBoolean(DoCreateStdParam(APrototype, vBoolean));
end;

function CreateBufferParam(APrototype: IPParamPrototype): IPBuffer;
begin
  Result:=IPBuffer(DoCreateStdParam(APrototype, vBuffer));
end;

function CreateStringParam(APrototype: IPParamPrototype): IPString;
begin
  Result:=IPString(DoCreateStdParam(APrototype, vString));
end;

function CreatePresetParam(APrototype: IPParamPrototype): IPPreset;
begin
  Result:=IPPreset(DoCreateStdParam(APrototype, vPreset));
end;

function CreatePresetParam(APrototype: IPParamPrototype; AEnvironment: IPVisualisationEnvironment): IPPreset;
var
  AParamSettings: IPParamSettings;
begin
  AParamSettings:=DoCreateStdParamSettings(APrototype, vPreset);
  Result:=IPPreset(AParamSettings.Param);
  IPPresetSettings(AParamSettings).Environment:=AEnvironment;
end;

function CreatePointerParam(APrototype: IPParamPrototype): IPPointer;
begin
  Result:=IPPointer(DoCreateStdParam(APrototype, vPointer));
end;

end.

