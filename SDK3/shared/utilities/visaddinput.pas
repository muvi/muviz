unit VisAddInput;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, StdParamTypes, MStrings;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString);
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVInteger);
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVFloat);
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVColor);
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVBoolean);
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVBuffer);
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVString);
//needed when trying to set a shortstring or other strings...
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: string);
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVPreset);
procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVPointer);
procedure AddInput(AVisualisation: IPVisualisation; AID: TPParamID);

implementation

{%REGION AddInput methods}

procedure AddInput(AVisualisation: IPVisualisation; AName: IString);
begin
  //do nothing
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVInteger);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vInteger)];
  IPInteger(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVFloat);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vFloat)];
  IPFloat(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVColor);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vColor)];
  IPColor(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVBoolean);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vBoolean)];
  IPBoolean(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVBuffer);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vBuffer)];
  IPBuffer(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVString);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vString)];
  IPString(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: string);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vString)];
  IPString(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVPreset);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vPreset)];
  IPPreset(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AName: IString; AValue: TVPointer);
var
  AParam: IPParam;
begin
  AParam:=AVisualisation[ParamID(AName, vPointer)];
  IPPointer(AParam).Value:=AValue;
end;

procedure AddInput(AVisualisation: IPVisualisation; AID: TPParamID);
begin
  //do nothing
end;

{%ENDREGION}

end.

