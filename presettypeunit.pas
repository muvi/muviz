unit PresetTypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TagUnit, csl, Enumerators, VisType2, VisTypeUnit2,
  ParamType2, LinkedList, HashMap, MapKeys, PresetType, ParamNotificationList,
  DebugTools, SyncObjs, TagTypeImpl, TagType, StdParamTypes, StdTags, MStrings,
  StdTagGroups, VisTypeImpl, GlobalTVSP, TVSPIndexedValues, VisAddInput,
  Dialogs;

type
  TPPresets          = class (TAbstractVisualisations)
  private
    FAllPresetTags: TTagSet;
    FPresets      : TMap;
    FPresetLock   : TMultiReadExclusiveWriteSynchronizer;
    FNullPreset   : TVisualisation;
    function GetPresetOrNil(AID: TPPresetID): TVisualisation;
    //Warning: inefficient...
    function GetPreset(AName: IString): TVisualisation;
    function GetTVSPIndexedParam(AGroup: TGUID; AType: TPParamType; AName: string): TTVSPIndexedValue;
  protected
    function GetAllPresetTags: TTagSet; override;
    procedure Remove(APreset: TCompleteVisualisation); override;
    procedure Added(AVisualisation: TCompleteVisualisation); override;
    function GetPreset(AID: TPPresetID): TVisualisation; override;
    function GetNullPreset: TVisualisation; override;
  public
    constructor Create(AVisTypes: TPTypes);
    destructor Destroy; override;
    function Add(AID: TPPresetID): TVisualisation;
    procedure Clear;
    property Items[AID: TPPresetID]: TVisualisation read GetPreset; default;
    //Warning: inefficient...
    property ItemsByName[AName: IString]: TVisualisation read GetPreset;
  end;

implementation

{%REGION TPPresets}

constructor TPPresets.Create(AVisTypes: TPTypes);
begin
  inherited Create(AVisTypes);
  FAllPresetTags:=TTagSet.Create;
  FPresets:=THashMap.Create;
  FPresetLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  //add the null preset
  FNullPreset:=TVisualisation.CreateZero(Self);
  FNullPreset.AddTag(TAGSYSTEM);
  FPresets.Add(TGUIDKey.Create(NULLPRESETID), FNullPreset);
  //AddInput(FNullPreset, NAMEINPUTNAME, NULLPRESETNAME);
  TVSP.OnCreateIndex:=@GetTVSPIndexedParam;
end;

destructor TPPresets.Destroy;
begin
  //do not destroy FNullPreset. This is done in Clear below.
  Clear;
  FPresets.Destroy;
  FPresetLock.Destroy;
  FAllPresetTags.Destroy;
  inherited Destroy;
end;

function TPPresets.Add(AID: TPPresetID): TVisualisation;
begin
  Result:=TVisualisation.Create(Self, AID);
end;

procedure TPPresets.Added(AVisualisation: TCompleteVisualisation);
begin
  FPresetLock.Beginwrite;
  FPresets.Add(TGUIDKey.Create(AVisualisation.ID), AVisualisation);
  FPresetLock.Endwrite;

  TVSP.Subscribe(AVisualisation.ID);
end;

function TPPresets.GetNullPreset: TVisualisation;
begin
  Result:=FNullPreset;
end;

function TPPresets.GetAllPresetTags: TTagSet;
begin
  Result:=FAllPresetTags;
end;

procedure TPPresets.Remove(APreset: TCompleteVisualisation);
var
  AKey: TObject;
begin
  AKey:=TGUIDKey.Create(APreset.ID);

  FPresetLock.Beginwrite;
  FPresets.Remove(AKey);
  APreset.Deleted;
  FPresetLock.Endwrite;

  AKey.Destroy;
end;

function TPPresets.GetPresetOrNil(AID: TPPresetID): TVisualisation;
var
  AKey : TObject;
  AItem: TObject;
begin
  AKey:=TGUIDKey.Create(AID);

  FPresetLock.Beginread;
  AItem:=FPresets.Items[AKey];
  FPresetLock.Endread;

  Result:=AItem as TVisualisation;

  AKey.Destroy;
end;

function TPPresets.GetPreset(AID: TPPresetID): TVisualisation;
begin
  Result:=GetPresetOrNil(AID);
  if Result = nil
    then Result:=Add(AID);
end;

function TPPresets.GetPreset(AName: IString): TVisualisation;
var
  AObject: TObject;
begin
  FPresetLock.Beginread;

  for AObject in FPresets do begin
    Result:=TVisualisation(AObject);
    if IPString(Result.Inputs[ParamID(NAMEINPUTNAME, vString)]).Value.Equals(AName) then begin
      FPresetLock.Endread;
      exit;
    end;
  end;

  FPresetLock.Endread;

  Result:=nil;
end;

function TPPresets.GetTVSPIndexedParam(AGroup: TGUID; AType: TPParamType; AName: string): TTVSPIndexedValue;
begin
  Result:=Items[AGroup].IndexedInputs[ParamID(AName, AType)];
end;

procedure TPPresets.Clear;
var
  AItem: TObject;
begin
  FPresetLock.Beginwrite;
  for AItem in FPresets
    do TVisualisation(AItem).Deleted;
  FPresets.Clear;
  FPresetLock.Endwrite;
end;

{%ENDREGION}

end.

