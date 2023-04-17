unit VisPresetList3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, StdParamTypes, VisEventImpl, MStrings,
  PresetType, VisAddInput, StdTags, ParamType2, ParamTypes, SimpleVis,
  VisualisationParamOrder, VisualisationParamOrderImpl, PointerBoundParamOrder,
  UniqueStrings, AdvParamType, VisualisationUtils;

type

  TPresetList        = class (TVisualisationEvents)
  strict private
    function CreatePointer: IPPointer;
  protected
    procedure GetInput(ASettings: IPParamSettings); cdecl; override;
    procedure GotInput(Param: IPParam); cdecl; override;
  public
    constructor Create(APrototype: IPVisualisationPrototype);
    destructor Destroy; override;
    procedure Suspend; cdecl; override;
    procedure Resume; cdecl; override;
    property Prototype;
  end;

const
  VIDPRESETLIST : TGUID = '{FFE17EDB-82D9-4BC4-A9A4-CF449A472F4F}';

procedure Register;
function MAININPUTID: TPParamID; inline;
function EMPTYINPUTID: TPParamID; inline;

implementation

{%REGION Notifications}

procedure PointerChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
var
  AValue: TVPointer;
begin
  Assert(Context <> nil);
  Assert(Sender <> nil);
  Assert(IPParam(Sender).ID.&Type = TPParamType(vPointer));
  with TPresetList(Context) do begin
    AValue:=IChangedPointer(SenderData).Get;
    if AValue.Output.Preset.Name.Equals(EmptyString)
      and (AValue.Output.Param = ParamID(MAININPUTNAME, vCall))
      and (AValue.Input.Param = ParamID(MAININPUTNAME, vCall))
      and (AValue.Input.Preset.&Type = vPreset)
        then IPointerBoundParamOrder(PresetInputs[AValue.Input.Preset.Name].AttachedInterfaces[GUIDVISPARAMORDER]).BoundTo:=IPPointer(Sender);
  end;
end;

{%ENDREGION}
{%REGION TPresetList}

constructor TPresetList.Create(APrototype: IPVisualisationPrototype);
begin
  inherited Create(APrototype);
end;

destructor TPresetList.Destroy;
begin
  inherited Destroy;
end;

procedure TPresetList.Suspend; cdecl;
var
  AIterator : Pointer;
  AParam    : IPParam;
begin
  AIterator:=nil;
  AParam:=Prototype.IterateInput(AIterator);
  while AParam<>nil do begin
    if AParam.ID.&Type = TPParamType(vPreset)
      then PresetUtil[IPPreset(AParam).ExecutedValue].Suspend;
    AParam:=Prototype.IterateInput(AIterator);
  end;
end;

procedure TPresetList.Resume; cdecl;
var
  AIterator : Pointer;
  AParam    : IPParam;
begin
  AIterator:=nil;
  AParam:=Prototype.IterateInput(AIterator);
  while AParam<>nil do begin
    if AParam.ID.&Type = TPParamType(vPreset)
      then PresetUtil[IPPreset(AParam).ExecutedValue].Resume;
    AParam:=Prototype.IterateInput(AIterator);
  end;
end;

procedure TPresetList.GetInput(ASettings: IPParamSettings); cdecl;
begin
  if ASettings.Param.ID.&Type = TPParamType(vPreset) then begin
    IPPresetSettings(ASettings).Environment:=Prototype.Environment.CreateSubEnvironment;
    ASettings.Param.AttachInterface(IPointerBoundParamOrder(TPointerBoundParamOrder.Create(ASettings.Param, PointerSide(ASettings.Param.ID, MAININPUTID), PointerSide(EMPTYINPUTID, MAININPUTID), MaxInt-1, @CreatePointer)));
  end else inherited GetInput(ASettings);
end;

procedure TPresetList.GotInput(Param: IPParam); cdecl;
begin
  if Param.ID.&Type <> TPParamType(vPreset)
    then if Param.ID = MAININPUTID
      then Param.AttachInterface(IVisualisationParamOrder(TFixedVisualisationParamOrder.Create(not MaxInt)))
      else Param.AttachInterface(IVisualisationParamOrder(TFixedVisualisationParamOrder.Create(MaxInt)));
  if Param.ID.&Type = TPParamType(vPointer)
    then Param.AddListener(@PointerChanged, Self, nil);
end;

function TPresetList.CreatePointer: IPPointer;
begin
  Result:=PointerInputs[GloballyUniqueEnglishText];
end;

{%ENDREGION}
{%REGION Misc}

procedure CreatePresetList(APrototype: IPVisualisationPrototype); cdecl;
begin
  TPresetList.Create(APrototype);
end;

procedure Register;
begin
  with PresetUtil do begin
    //TVPresetList
    RegisterVis(VIDPRESETLIST, @CreatePresetList);
    with CreatePreset('Empty presetlist 3', VIDPRESETLIST) do begin
      AddInput(This, 'Preset 1', DEFAULTPRESET);
      AddTag(TAGNONVISUAL);
      AddTag(TAGPREDEFINED);
      AddTag('Trigger.Structure');
      AddTag(TAGLISTED);
    end;
  end;
end;

function MAININPUTID: TPParamID; inline;
begin
  Result:=ParamID(MAININPUTNAME, vCall);
end;

function EMPTYINPUTID: TPParamID; inline;
begin
  Result:=ParamID('', vPreset);
end;

{%ENDREGION}

end.

