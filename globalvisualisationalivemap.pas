unit GlobalVisualisationAliveMap;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, VisType2, MStrings, PresetType, StdParamTypes, StdTags, GUIDop,
  VisAddInput, VisualisationUtils;

function Load(AParamOwnerID: TPPresetID; AParamName: IString; AID: TPPresetID; AEnvironment: IPVisualisationEnvironment): IPVisualisation;
procedure Buffer(AParamOwnerID: TPPresetID; AParamName: IString; AID: TPPresetID; AEnvironment: IPVisualisationEnvironment);
procedure Release(AParamOwnerID: TPPresetID; AParamName: IString; AID: TPPresetID);
procedure BufferAll(AParamOwnerID: TPPresetID; AParamName: IString; ATag: IString; AEnvironment: IPVisualisationEnvironment);
procedure BufferAll(AParamOwnerID: TPPresetID; AParamName: IString; AEnvironment: IPVisualisationEnvironment);
procedure ReleaseAll(AParamOwnerID: TPPresetID; AParamName: IString; ATag: IString);
procedure ReleaseAll(AParamOwnerID: TPPresetID; AParamName: IString);
procedure SetBufferedVersion(AParamOwnerID: TPPresetID; AParamName: IString; AID, ABufferedVersion: TPPresetID);

procedure Register;

implementation

var
  GlobalAliveMap: IPVisualisation = nil;

function FindInput(AParamOwnerID: TPPresetID; AParamName: IString; AID: TPPresetID): IPPreset;
begin
  Result:=IPPreset(GlobalAliveMap.Inputs[ParamID(GUIDToString(AParamOwnerID) + ' ' + AParamName + ' ' + GUIDToString(AID), vPreset)]);
end;

function Load(AParamOwnerID: TPPresetID; AParamName: IString; AID: TPPresetID; AEnvironment: IPVisualisationEnvironment): IPVisualisation;
var
  AResultParam: IPPreset;
  AResultID   : TPPresetID;
begin
  AResultParam:=FindInput(AParamOwnerID, AParamName, AID);
  AResultID:=AResultParam.Value;
  if AResultID <> NULLPRESETID then begin
    Result:=PresetUtil[AResultID];
    //ensure the environment is set correctly
    Result.Environment:=AEnvironment;
  end else begin
    CreateGUID(AResultID);
    Result:=MakeChild(AResultID, AID, true, AEnvironment);
    //TODO: maybe remove this because of introduction of ExecutedValue in IPPreset
    //set the mapping from the new preset to itself to prevent double buffering
    FindInput(AParamOwnerID, AParamName, AResultID).Value:=AResultID;
    //set the real mapping
    AResultParam.Value:=AResultID;
  end;
end;

procedure SetBufferedVersion(AParamOwnerID: TPPresetID; AParamName: IString; AID, ABufferedVersion: TPPresetID);
begin
  FindInput(AParamOwnerID, AParamName, AID).Value:=ABufferedVersion;
end;

procedure Buffer(AParamOwnerID: TPPresetID; AParamName: IString; AID: TPPresetID; AEnvironment: IPVisualisationEnvironment);
begin
  Load(AParamOwnerID, AParamName, AID, AEnvironment);
end;

procedure Release(AParamOwnerID: TPPresetID; AParamName: IString; AID: TPPresetID);
begin
  FindInput(AParamOwnerID, AParamName, AID).Value:=NULLPRESETID;
end;

procedure BufferAll(AParamOwnerID: TPPresetID; AParamName: IString; ATag: IString; AEnvironment: IPVisualisationEnvironment);
begin
  raise ENotImplemented.Create('BufferAll');
end;

procedure BufferAll(AParamOwnerID: TPPresetID; AParamName: IString; AEnvironment: IPVisualisationEnvironment);
begin
  raise ENotImplemented.Create('BufferAll');
end;

procedure ReleaseAll(AParamOwnerID: TPPresetID; AParamName: IString; ATag: IString);
begin
  raise ENotImplemented.Create('ReleaseAll');
end;

procedure ReleaseAll(AParamOwnerID: TPPresetID; AParamName: IString);
begin
  raise ENotImplemented.Create('ReleaseAll');
end;

const
  VIDPRESETALIVEMAP: TGUID = '{03698D43-A70D-445E-9B7C-1A884376547E}';

procedure Register;
begin
  GlobalAliveMap:=CreatePreset('Global Preset Alive Map', VIDPRESETALIVEMAP);
  with GlobalAliveMap do begin
    AddTag(TAGSYSTEM);
    AddTag(TAGHIDDEN);
    AddTag(TAGPREDEFINED);
    AddTag(TAGNONVISUAL);
  end;
end;

finalization
  GlobalAliveMap:=nil;
end.

