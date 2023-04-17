unit ValueStorages;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamType2, StdParamTypes, VisType2, MStrings,
  AdvParamType, PresetType, UniqueStrings;

function CreateValueStorage(AType: TPParamType): IPParam;

function CreateCallValueStorage: IPCall; //inline;
function CreateIntegerValueStorage(AValue: TVInteger): IPInteger; //inline;
function CreateFloatValueStorage(AValue: TVFloat): IPFloat; //inline;
function CreateColorValueStorage(AValue: TVColor): IPColor; //inline;
function CreateBooleanValueStorage(AValue: TVBoolean): IPBoolean; //inline;
function CreateBufferValueStorage(AValue: TVBuffer): IPBuffer; //inline;
function CreateStringValueStorage(AValue: TVString): IPString; //inline;
function CreatePresetValueStorage(AValue: TVPreset): IPPreset; //inline;
function CreatePointerValueStorage(AValue: TVPointer): IPPointer; //inline;

const
  PIDTEMPSTORAGE: TGUID = '{07C641E1-54DF-4BBF-B397-FABC518251C5}';

implementation

{%REGION Create Methods}

function CreateValueStorage(AType: TPParamType): IPParam;
begin
  Result:=PresetUtil[PIDTEMPSTORAGE][ParamID(GloballyUniqueEnglishText, AType)];
end;

function CreateCallValueStorage: IPCall; //inline;
begin
  Result:=IPCall(CreateValueStorage(vCall));
end;

function CreateIntegerValueStorage(AValue: TVInteger): IPInteger; //inline;
begin
  Result:=IPInteger(CreateValueStorage(vInteger));
  Result.&Set(AValue);
end;

function CreateFloatValueStorage(AValue: TVFloat): IPFloat; //inline;
begin
  Result:=IPFloat(CreateValueStorage(vFloat));
  Result.&Set(AValue);
end;

function CreateColorValueStorage(AValue: TVColor): IPColor; //inline;
begin
  Result:=IPColor(CreateValueStorage(vColor));
  Result.&Set(AValue);
end;

function CreateBooleanValueStorage(AValue: TVBoolean): IPBoolean; //inline;
begin
  Result:=IPBoolean(CreateValueStorage(vBoolean));
  Result.&Set(AValue);
end;

function CreateBufferValueStorage(AValue: TVBuffer): IPBuffer; //inline;
begin
  Result:=IPBuffer(CreateValueStorage(vBuffer));
  Result.&Set(AValue);
end;

function CreateStringValueStorage(AValue: TVString): IPString; //inline;
begin
  Result:=IPString(CreateValueStorage(vString));
  Result.&Set(AValue);
end;

function CreatePresetValueStorage(AValue: TVPreset): IPPreset; //inline;
begin
  Result:=IPPreset(CreateValueStorage(vPreset));
  Result.Value:=AValue;
end;

function CreatePointerValueStorage(AValue: TVPointer): IPPointer; //inline;
begin
  Result:=IPPointer(CreateValueStorage(vPointer));
  Result.&Set(AValue);
end;

{%ENDREGION}

end.

