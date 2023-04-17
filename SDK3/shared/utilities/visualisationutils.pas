unit VisualisationUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, TagType, PresetType, StdParamTypes, VisAddInput,
  MStrings, StdPermissions;

function CreatePreset(AID: TPPresetID; AEnvironment: IPVisualisationEnvironment = nil): IPVisualisation;
function CreatePreset(AName: IString; AID: TPPresetID; AEnvironment: IPVisualisationEnvironment = nil): IPVisualisation;
procedure CopyVis(ASrc, ADest: IPVisualisation);
function MakeChild(AID: TPPresetID; AParent: TPPresetID; ALoaded: Boolean = false; AEnvironment: IPVisualisationEnvironment = nil; AName: IString = nil): IPVisualisation;
function MakeChild(AParent: TPPresetID; ALoaded: Boolean = false; AEnvironment: IPVisualisationEnvironment = nil; AName: IString = nil): IPVisualisation;
function CopyableParam(AID: TPParamID): Boolean;
procedure SaveToParent(AVisualisation: IPVisualisation);
function MakeTemplate(AID: TPPresetID): IPVisualisation;

implementation

function CreatePreset(AID: TPPresetID; AEnvironment: IPVisualisationEnvironment = nil): IPVisualisation;
begin
  Result:=PresetUtil[AID];
  if AEnvironment <> nil
    then Result.Environment:=AEnvironment
    else Result.Environment:=VisualisationUtil.MainEnvironment;
end;

function CreatePreset(AName: IString; AID: TPPresetID; AEnvironment: IPVisualisationEnvironment = nil): IPVisualisation;
begin
  Result:=CreatePreset(AID, AEnvironment);
  AddInput(Result, NAMEINPUTNAME, AName);
end;

procedure CopyVis(ASrc, ADest: IPVisualisation);
var
  AParam   : IPParam;
  AIterator: Pointer;
  I        : Integer;
  ATags    : ITags;
begin
  Assert(ASrc <> nil);
  Assert(ADest <> nil);
  //copy params
  AIterator:=nil;
  AParam:=ASrc.IterateInput(AIterator);
  while AParam<>nil do begin
    //copy value
    if CopyableParam(AParam.ID)
      then ADest[AParam.ID].GetFrom(AParam, TPNOLIMIT, NULLVISID);
    //increment
    AParam:=ASrc.IterateInput(AIterator);
  end;
  //copy tags
  ATags:=ASrc.Tags;
  for I:=0 to ATags.Count-1
    do ADest.AddTag(ATags[I]);
end;

function MakeChild(AID: TPPresetID; AParent: TPPresetID; ALoaded: Boolean = false; AEnvironment: IPVisualisationEnvironment = nil; AName: IString = nil): IPVisualisation;
var
  AParentVis: IPVisualisation;
begin
  AParentVis:=PresetUtil[AParent];
  if AName = nil
    then AName:='Copy of ' + IPString(AParentVis[ParamID(NAMEINPUTNAME, vString)]).Value;
  Result:=CreatePreset(AID, AEnvironment);
  CopyVis(AParentVis, Result);
  with Result do begin
    AddInput(This, NAMEINPUTNAME, AName);
    AddInput(This, PARENTINPUTNAME, AParent);
    AddInput(This, LOADEDINPUTNAME, ALoaded);
  end;
end;

function MakeChild(AParent: TPPresetID; ALoaded: Boolean = false; AEnvironment: IPVisualisationEnvironment = nil; AName: IString = nil): IPVisualisation;
var
  AID: TGUID;
begin
  CreateGUID(AID);
  Result:=MakeChild(AID, AParent, ALoaded, AEnvironment, AName);
end;

function MakeTemplate(AID: TPPresetID): IPVisualisation;
var
  AVis  : IPVisualisation;
  ANewID: TGUID;
begin
  AVis:=PresetUtil[AID];
  CreateGUID(ANewID);
  Result:=CreatePreset(ANewID);
  CopyVis(AVis, Result);
  with Result do begin
    AddInput(This, NAMEINPUTNAME, IPString(AVis[ParamID(NAMEINPUTNAME, vString)]).Value);
    AddInput(This, PARENTINPUTNAME, IPPreset(AVis[ParamID(PARENTINPUTNAME, vPreset)]).Value);
    AddInput(This, LOADEDINPUTNAME, false);
  end;
end;

function CopyableParam(AID: TPParamID): Boolean;
begin
  Result:=(AID <> ParamID(PARENTINPUTNAME, vPreset))
    and (AID <> ParamID(LOADEDINPUTNAME, vBoolean))
    and (AID <> ParamID(NAMEINPUTNAME, vString));
end;

procedure SaveToParent(AVisualisation: IPVisualisation);
begin
  CopyVis(AVisualisation, AVisualisation.Parent);
end;

end.

