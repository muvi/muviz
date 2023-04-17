unit VisTypeImpl;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, StdParamTypes, csl, HashMap, GUIDop,
  ParamMap, SyncObjs, VisEventImpl, MStrings, StdTagGroups,
  ParamNotificationList, MExceptions, TagType, TagUnit, TVSPIndexedValues,
  TagTypeImpl, Enumerators, VisTypeUnit2, VisAddInput, PresetType, ParamType2,
  SimpleNotificationList, LogUnit, StdPermissions;

type
  TParamInitMethod       = procedure (Input: IPParam) of object;

  TBasicVisualisation    = class (TVTVSPGroupable, IPVisualisationPrototype)
  strict private
    FEnvironment          : IPVisualisationEnvironment;
    FNewEnvironment       : IPVisualisationEnvironment;
    FEvents               : IPVisualisationEvents;
    //Restart Critical Information lock
    FRCILock              : TMultiReadExclusiveWriteSynchronizer;
    FNewEnvironmentLock   : TMultiReadExclusiveWriteSynchronizer;
    FInputMap             : TVParamMap;
    FSuspendedCount       : Integer;
    FSuspendedLock        : TCriticalSection;
    FInputGotNotifications: TNotificationList;
    function _PrototypeAddRefRelease: Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function IPVisualisationPrototype._AddRef = _PrototypeAddRefRelease;
    function IPVisualisationPrototype._Release = _PrototypeAddRefRelease;

    function GetEvents: IPVisualisationEvents; inline;

    procedure InitParam(AParamSettings: IPParamSettings; AInitMethod: TParamInitMethod);
    procedure ReinsertInputs;
  private
    procedure ReinsertedInput(Input: IPParam);
  strict protected
    function GetID: TPPresetID; cdecl; virtual; abstract;
    procedure DoInitParam(AParamSettings: IPParamSettings); virtual;
    procedure Restart; virtual; abstract;
  protected
    //for debugging only!
    function GetName: string;

    function RequestParam(APrototype: IPParamPrototype): IPParamSettings; override;
    procedure InitParam(AParamSettings: IPParamSettings); override;

    function GetInput(AID: TPParamID): IPParam; cdecl;
    function GetIndexedInput(AID: TPParamID): TTVSPIndexedValue;
    procedure GotInput(Input: IPParam); virtual;
    procedure SetEvents(AEvents: IPVisualisationEvents); cdecl;
    function GetEnvironment: IPVisualisationEnvironment; cdecl;
    procedure SetEnvironment(AEnvironment: IPVisualisationEnvironment); cdecl;
    function GetThread: IPThread; cdecl;
    //This Thread If Nil
    function ttin(AThread: IPThread): IPThread; inline;
    property Environment: IPVisualisationEnvironment read GetEnvironment write SetEnvironment;
    property Events: IPVisualisationEvents read GetEvents;
    property InputMap: TVParamMap read FInputMap;
    property Thread: IPThread read GetThread;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Suspend; cdecl;
    procedure Resume; cdecl;
    procedure AddInputGotListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread = nil); cdecl;
    procedure RemoveInputGotListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
    function IterateInput(var AIterator: Pointer): IPParam; cdecl;

    property IndexedInputs[AID: TPParamID]: TTVSPIndexedValue read GetIndexedInput;
    property Inputs[AID: TPParamID]: IPParam read GetInput; default;
  end;

  TCompleteVisualisation = class;

  TAbstractVisualisations= class
  strict private
    FVisTypes: TPTypes;
  protected
    procedure Remove(AVisualisation: TCompleteVisualisation); virtual; abstract;
    function GetAllPresetTags: TTagSet; virtual; abstract;
    procedure Added(AVisualisation: TCompleteVisualisation); virtual; abstract;
    function GetPreset(AID: TPPresetID): TCompleteVisualisation; virtual; abstract;
    function GetNullPreset: TCompleteVisualisation; virtual; abstract;
  public
    constructor Create(AVisTypes: TPTypes);
    property AllPresetTags: TTagSet read GetAllPresetTags;
    property VisTypes: TPTypes read FVisTypes;
    property NullPreset: TCompleteVisualisation read GetNullPreset;
  end;

  TCompleteVisualisation = class (TBasicVisualisation, IPVisualisation)
  strict private
    FIsDeleted              : Boolean;
    FInterface              : IPVisualisation;
    FTags                   : TTags;
    FOwner                  : TAbstractVisualisations;
    FID                     : TPPresetID;

    FVisualises             : TPType;
    FVisualisesConst        : Boolean;
    FVisualisesNotifications: TSimpleNotificationList;

    FParent                 : TCompleteVisualisation;

    FLoaded                 : Cardinal;
    procedure DoCreate; inline;
    function GetTags: ITags; cdecl;

    function GetParentID: TPPresetID;
    function GetParent: IPVisualisation; cdecl;
    function GetParentLocal: TCompleteVisualisation;
    procedure SetParentLocal(AParent: TCompleteVisualisation);

    function GetVisualisesLocal: TPType;
    procedure SetVisualisesLocal(AVisualises: TPType);
    function GetVisualises: TPVisID; cdecl;
    procedure VisualisesListener;

    function IsLoaded: Boolean; cdecl;
    procedure SetLoaded(ALoaded: Boolean);
  private
    procedure CopyFrom(AOther: TCompleteVisualisation); deprecated;
    procedure AddVisualisesListener(AMethod: TSimpleNotification);
    procedure RemoveVisualisesListener(AMethod: TSimpleNotification);
  strict protected
    function GetID: TPPresetID; cdecl; override;
    procedure DoInitParam(AParamSettings: IPParamSettings); override;
    procedure Restart; override;
    function GetGroup: TGUID; override;
    function GetVisualisation: IPVisualisation; override;
    property VisualisesLocal: TPType read GetVisualisesLocal write SetVisualisesLocal;
  protected
    property Owner: TAbstractVisualisations read FOwner;
  public
    constructor CreateZero(AOwner: TAbstractVisualisations);
    constructor Create(AOwner: TAbstractVisualisations; AID: TPPresetID);
    destructor Destroy; override;
    procedure AddTag(ATag: IString); cdecl;
    procedure RemoveTag(ATag: IString); cdecl;
    function HasTag(ATag: IString): Boolean; cdecl;
    function IsDeleted: Boolean; cdecl;
    procedure Delete; cdecl;
    //called by TPPresets
    procedure Deleted;

    function This: IPVisualisation; cdecl;

    property ID: TPPresetID read GetID;
    property Loaded: Boolean read IsLoaded write SetLoaded;
    property ParentLocal: TCompleteVisualisation read GetParentLocal write SetParentLocal;
    property ParentID: TPPresetID read GetParentID;
    property Tags: ITags read GetTags;
    property Visualises: TPVisID read GetVisualises;
  end;

  //does the pointer stuff automatically
  TPointerVisualisation  = class (TCompleteVisualisation)
  protected
    procedure GotInput(Input: IPParam); override;
  end;

  TVisualisation         = TPointerVisualisation;

const
  VARIABLEPREFIX = 'G';

implementation

var
  DefaultEvents: IPVisualisationEvents;

{%REGION TBasicVisualisation - helper methods}

procedure EventsInputGot(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  Assert(Sender <> nil);
  Assert(SenderData = nil);
  Assert(Context <> nil);
  TBasicVisualisation(Context).Events.GotInput(IPParam(Sender));
end;

function SpecialPurposeParam(AID: TPParamID): Boolean;
begin
  Result:=(AID = ParamID(LOADEDINPUTNAME, vBoolean))
    or (AID = ParamID(PARENTINPUTNAME, vPreset));
end;

{%ENDREGION}
{%REGION TBasicVisualisation}

constructor TBasicVisualisation.Create;
begin
  inherited Create;
  FSuspendedCount:=1;
  FSuspendedLock:=TCriticalSection.Create;
  FInputGotNotifications:=TNotificationList.Create;
  FEnvironment:=VisualisationUtil.MainEnvironment;
  FNewEnvironment:=FEnvironment;
  FRCILock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FNewEnvironmentLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FEvents:=DefaultEvents;
  FInputMap:=TVParamMap.Create(Self);
end;

destructor TBasicVisualisation.Destroy;
begin
  FInputMap.Destroy;
  FEvents:=nil;
  FNewEnvironmentLock.Destroy;
  FRCILock.Destroy;
  FInputGotNotifications.Destroy;
  FSuspendedLock.Destroy;
  inherited Destroy;
end;

procedure TBasicVisualisation.AddInputGotListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread = nil); cdecl;
begin
  FInputGotNotifications.Add(AListener, AContext, ttin(AThread));
end;

procedure TBasicVisualisation.RemoveInputGotListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  FInputGotNotifications.Remove(AListener, AContext);
end;

function TBasicVisualisation.GetInput(AID: TPParamID): IPParam; cdecl;
begin
  Result:=FInputMap[AID];
end;

function TBasicVisualisation.GetIndexedInput(AID: TPParamID): TTVSPIndexedValue;
begin
  Result:=FInputMap.IndexedParams[AID];
end;

procedure TBasicVisualisation.ReinsertedInput(Input: IPParam);
begin
  if not SpecialPurposeParam(Input.ID)
    then Thread.Push(@EventsInputGot, Self, Input, nil);
end;

procedure TBasicVisualisation.GotInput(Input: IPParam);
begin
  ReinsertedInput(Input);
  FInputGotNotifications.Notify(Input, nil);
end;

procedure TBasicVisualisation.SetEvents(AEvents: IPVisualisationEvents); cdecl;
begin
  FSuspendedLock.Enter;
  if FSuspendedCount < 1
    then FEvents.Suspend;

  FRCILock.Beginwrite;

  FNewEnvironmentLock.Beginread;
  FEnvironment:=FNewEnvironment;
  FNewEnvironmentLock.Endread;

  FEvents:=AEvents;
  FRCILock.Endwrite;

  ReinsertInputs;

  if FSuspendedCount < 1
    then Events.Resume;
  FSuspendedLock.Leave;
end;

function TBasicVisualisation.GetEnvironment: IPVisualisationEnvironment; cdecl;
begin
  FRCILock.Beginread;
  Result:=FEnvironment;
  FRCILock.Endread;
end;

procedure TBasicVisualisation.SetEnvironment(AEnvironment: IPVisualisationEnvironment); cdecl;
var
  AChanged: Boolean;
begin
  Assert(AEnvironment <> nil);
  Log('VI::' + GetName + ': Environment changed');

  FNewEnvironmentLock.Beginwrite;
  AChanged:=AEnvironment <> FNewEnvironment;
  if AChanged
    then FNewEnvironment:=AEnvironment;
  FNewEnvironmentLock.Endwrite;

  if AChanged
    then Restart;
end;

function TBasicVisualisation.GetThread: IPThread; cdecl;
begin
  Result:=Environment.Thread;
end;

procedure TBasicVisualisation.Suspend; cdecl;
begin
  FSuspendedLock.Enter;

  Inc(FSuspendedCount);
  if FSuspendedCount{InterLockedIncrement(FSuspendedCount)} = 1
    then Events.Suspend;

  FSuspendedLock.Leave;
end;

procedure TBasicVisualisation.Resume; cdecl;
begin
  FSuspendedLock.Enter;

  Dec(FSuspendedCount);
  if FSuspendedCount{InterLockedDecrement(FSuspendedCount)} = 0
    then Events.Resume;

  FSuspendedLock.Leave;
end;

function TBasicVisualisation.IterateInput(var AIterator: Pointer): IPParam; cdecl;
var
  AResult: IPParamSettings;
begin
  AResult:=FInputMap.Iterate(TEnumerator(AIterator));
  if AResult <> nil
    then Result:=AResult.Param
    else Result:=nil;
end;

procedure TBasicVisualisation.DoInitParam(AParamSettings: IPParamSettings);
begin
  //do nothing
end;

procedure TBasicVisualisation.InitParam(AParamSettings: IPParamSettings; AInitMethod: TParamInitMethod);
begin
  Assert(AParamSettings <> nil);
  if not SpecialPurposeParam(AParamSettings.Param.ID)
    then Events.GetInput(AParamSettings);

  AInitMethod(AParamSettings.Param);
end;

procedure TBasicVisualisation.InitParam(AParamSettings: IPParamSettings);
begin
  InitParam(AParamSettings, @GotInput);
end;

function TBasicVisualisation.RequestParam(APrototype: IPParamPrototype): IPParamSettings;
begin
  Assert(APrototype <> nil);
  Result:=ParamTypeUtil[APrototype.ID.&Type].CreateValueStorage(APrototype);
  DoInitParam(Result);
end;

procedure TBasicVisualisation.ReinsertInputs;
var
  AEnumerator   : TEnumerator;
  AParamSettings: IPParamSettings;
begin
  AEnumerator:=nil;
  AParamSettings:=FInputMap.Iterate(AEnumerator);
  while AParamSettings <> nil do begin
    InitParam(AParamSettings, @ReinsertedInput);
    AParamSettings:=FInputMap.Iterate(AEnumerator);
  end;
end;

function TBasicVisualisation._PrototypeAddRefRelease: Longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result:=1;
end;

function TBasicVisualisation.ttin(AThread: IPThread): IPThread; inline;
begin
  if AThread=nil
    then Result:=Thread
    else Result:=AThread;
end;

function TBasicVisualisation.GetEvents: IPVisualisationEvents; inline;
begin
  FRCILock.Beginread;
  Result:=FEvents;
  FRCILock.Endread;
end;

function TBasicVisualisation.GetName: string;
begin
  Result:=IPString(Inputs[ParamID(NAMEINPUTNAME, vString)]).Value;
end;

{%ENDREGION}
{%REGION TAbstractVisualisations}

constructor TAbstractVisualisations.Create(AVisTypes: TPTypes);
begin
  Assert(AVisTypes <> nil);
  inherited Create;
  FVisTypes:=AVisTypes;
end;

{%ENDREGION}
{%REGION TCompleteVisualisation - helper methods}

procedure LoadedInputChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  TCompleteVisualisation(Context).Loaded:=IChangedBoolean(SenderData).Get;
end;

procedure ParentInputChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TCompleteVisualisation(Context)
    do ParentLocal:=Owner.GetPreset(IChangedPreset(SenderData).Get);
end;

{%ENDREGION}
{%REGION TCompleteVisualisation}

procedure TCompleteVisualisation.DoCreate; inline;
begin
  FInterface:=IPVisualisation(Self);
  FTags:=TTags.Create(FOwner.AllPresetTags, Self);
  FVisualisesNotifications:=TSimpleNotificationList.Create;
  FOwner.Added(Self);
end;

constructor TCompleteVisualisation.CreateZero(AOwner: TAbstractVisualisations);
begin
  inherited Create;
  FLoaded:=Cardinal(false);
  FParent:=Self;
  FOwner:=AOwner;
  FID:=NULLPRESETID;
  FVisualises:=AOwner.VisTypes.NullVis;
  FVisualisesConst:=true;
  FIsDeleted:=false;
  DoCreate;
end;

constructor TCompleteVisualisation.Create(AOwner: TAbstractVisualisations; AID: TPPresetID);
begin
  //use CreateZero instead!
  Assert(AID <> '{00000000-0000-0000-0000-000000000000}');
  Assert(AOwner <> nil);

  inherited Create;
  FLoaded:=Cardinal(false);
  FParent:=AOwner.NullPreset;
  FParent._AddRef;
  FOwner:=AOwner;
  FID:=AID;
  FVisualises:=FOwner.VisTypes.VisualisationsOrNil[AID];
  FVisualisesConst:=FVisualises <> nil;
  if not FVisualisesConst then begin
    FVisualises:=FParent.VisualisesLocal;
    FParent.AddVisualisesListener(@VisualisesListener);
  end;
  Assert(FVisualises <> nil);
  FIsDeleted:=false;
  DoCreate;
end;

destructor TCompleteVisualisation.Destroy;
begin
  Assert(FParent <> nil);
  FParent._Release;
  FTags.Destroy;
  FVisualisesNotifications.Destroy;
  inherited Destroy;
end;

procedure TCompleteVisualisation.Restart;
begin
  if Loaded
    then VisualisesLocal.&Constructor(Self)
    else SetEvents(DefaultEvents);
end;

procedure TCompleteVisualisation.CopyFrom(AOther: TCompleteVisualisation);
var
  AParam, ANewParam: IPParam;
  AIterator        : Pointer;
  I                : Integer;
  ATags            : ITags;
begin
  //copy params
  AIterator:=nil;
  AParam:=AOther.IterateInput(AIterator);
  while AParam<>nil do begin
    ANewParam:=Inputs[AParam.ID];
    //copy value
    ANewParam.GetFrom(AParam, TPNOLIMIT, NULLVISID);
    //increment
    AParam:=AOther.IterateInput(AIterator);
  end;
  //copy tags
  ATags:=AOther.Tags;
  for I:=0 to ATags.Count-1
    do AddTag(ATags[I]);
end;

procedure TCompleteVisualisation.AddTag(ATag: IString); cdecl;
begin
  FTags += PresetTags[ATag];
end;

procedure TCompleteVisualisation.RemoveTag(ATag: IString); cdecl;
begin
  FTags -= PresetTags[ATag];
end;

function TCompleteVisualisation.HasTag(ATag: IString): Boolean; cdecl;
begin
  Result:=FTags.Has(PresetTags[ATag]);
end;

function TCompleteVisualisation.GetTags: ITags; cdecl;
begin
  Result:=ITags(TITags.Create(FTags.ToArray, PresetTags));
end;

function TCompleteVisualisation.IsDeleted: Boolean; cdecl;
begin
  Result:=FIsDeleted;
end;

procedure TCompleteVisualisation.Delete; cdecl;
begin
  Assert(not FIsDeleted);
  FOwner.Remove(Self);
end;

procedure TCompleteVisualisation.Deleted;
begin
  FInterface:=nil;
  FIsDeleted:=true;
end;

function TCompleteVisualisation.IsLoaded: Boolean; cdecl;
var
  AResult: Cardinal;
begin
  InterLockedExchange(AResult, FLoaded);
  Result:=Boolean(AResult);
end;

procedure TCompleteVisualisation.SetLoaded(ALoaded: Boolean);
var
  AOldLoaded: Boolean;
begin
  AOldLoaded:=Boolean(InterLockedExchange(FLoaded, Cardinal(ALoaded)));
  Log('VI::' + GetName + ': Loaded: ' + BoolToStr(AOldLoaded) + ' -> ' + BoolToStr(ALoaded));
  if AOldLoaded <> ALoaded then begin
    if ALoaded
      then VisualisesLocal.&Constructor(Self)
      else SetEvents(DefaultEvents);
  end;
end;

procedure TCompleteVisualisation.DoInitParam(AParamSettings: IPParamSettings);
begin
  with AParamSettings.Param do begin
    if ID = ParamID(LOADEDINPUTNAME, vBoolean)
      then AddListener(@LoadedInputChanged, Self, Thread);
    if ID = ParamID(PARENTINPUTNAME, vPreset)
      then AddListener(@ParentInputChanged, Self, Thread);
  end;
end;

function TCompleteVisualisation.GetParent: IPVisualisation; cdecl;
begin
  Result:=ParentLocal;
end;

function TCompleteVisualisation.GetParentLocal: TCompleteVisualisation;
begin
  InterLockedExchange(Pointer(Result), FParent);
end;

procedure TCompleteVisualisation.SetParentLocal(AParent: TCompleteVisualisation);
var
  AOldParent: TCompleteVisualisation;
begin
  AParent._AddRef;

  AOldParent:=TCompleteVisualisation(InterLockedExchange(Pointer(FParent), AParent));
  Log('VI::' + GetName + ': Parent: ' + AOldParent.GetName + ' -> ' + AParent.GetName);
  if (not FVisualisesConst) and (AOldParent <> AParent) then begin
    AOldParent.RemoveVisualisesListener(@VisualisesListener);
    AParent.AddVisualisesListener(@VisualisesListener);
    VisualisesLocal:=AParent.VisualisesLocal;
  end;

  AOldParent._Release;
end;

function TCompleteVisualisation.This: IPVisualisation; cdecl;
begin
  Result:=Self;
end;

function TCompleteVisualisation.GetID: TPPresetID; cdecl;
begin
  Result:=FID;
end;

function TCompleteVisualisation.GetParentID: TPPresetID;
begin
  if ParentLocal <> nil then begin
    Result:=ParentLocal.ID;
  end else begin
    Assert(ID = NULLPRESETID);
    Result:=ID;
  end;
end;

function TCompleteVisualisation.GetVisualisesLocal: TPType;
begin
  InterLockedExchange(Pointer(Result), FVisualises);
end;

procedure TCompleteVisualisation.SetVisualisesLocal(AVisualises: TPType);
var
  AOldVisualises: TPType;
begin
  Assert(not FVisualisesConst);
  AOldVisualises:=TPType(InterLockedExchange(Pointer(FVisualises), AVisualises));
  Log('VI::' + GetName + ': Visualises: ' + GUIDToString(AOldVisualises.ID) + '-> ' + GUIDToString(AVisualises.ID));
  if (AOldVisualises <> AVisualises) and isLoaded then begin
    AVisualises.&Constructor(Self);
    FVisualisesNotifications.Notify;
  end;
end;

function TCompleteVisualisation.GetVisualises: TPVisID; cdecl;
begin
  Result:=VisualisesLocal.ID;
end;

procedure TCompleteVisualisation.VisualisesListener;
begin
  Assert(not FVisualisesConst);
  VisualisesLocal:=ParentLocal.VisualisesLocal;
end;

procedure TCompleteVisualisation.AddVisualisesListener(AMethod: TSimpleNotification);
begin
  FVisualisesNotifications.Add(AMethod);
end;

procedure TCompleteVisualisation.RemoveVisualisesListener(AMethod: TSimpleNotification);
begin
  FVisualisesNotifications.Remove(AMethod);
end;

function TCompleteVisualisation.GetGroup: TGUID;
begin
  Result:=FID;
end;

function TCompleteVisualisation.GetVisualisation: IPVisualisation;
begin
  Result:=IPVisualisation(Self);
end;

{%ENDREGION}
{%REGION TLPointerInfo}

type
  TLPointerInfo = class (TInterfacedObject)
  private
    FVisualisation: TVisualisation;
    FCurrentValue : TVPointer;
    FOutputVis    : IPVisualisation;
    FInputVis     : IPVisualisation;
    //may not delete output and input while connected...
    FInput        : IPParam;
    FOutput       : IPParam;
    FInputPreset  : IPPreset;
    FOutputPreset : IPPreset;

    FLock         : TCriticalSection;
    procedure DisconnectOutput(const AValue: TVPointer);
    procedure DoConnect;
    procedure DoDisconnect;

    procedure UpdateInput;
    procedure UpdateOutput;

    procedure InitInputInPreset;
    procedure InitOutputInPreset;
    procedure InitInputSingle; inline;
    procedure InitOutputSingle; inline;

    function PointerConnectionMode: TVPointerConnectionMode;
  protected
    property CurrentValue: TVPointer read FCurrentValue;
    property Lock: TCriticalSection read FLock;
  public
    constructor Create(AParam: IPPointer; AVisualisation: TVisualisation);
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;
    procedure Update(AParam: IPPointer);
  end;

procedure LPointerChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TLPointerInfo(Context) do begin
    Lock.Enter;
    Update(IPPointer(Sender));
    Lock.Leave;
  end;
end;

procedure LInputPresetChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TLPointerInfo(Context) do begin
    Lock.Enter;

    DoDisconnect;
    UpdateInput;
    DoConnect;

    Lock.Leave;
  end;
end;

procedure LOutputPresetChanged(Context: Pointer; Sender, SenderData: IInterface); cdecl;
begin
  with TLPointerInfo(Context) do begin
    Lock.Enter;

    DoDisconnect;
    UpdateOutput;
    DoConnect;

    Lock.Leave;
  end;
end;

constructor TLPointerInfo.Create(AParam: IPPointer; AVisualisation: TVisualisation);
begin
  inherited Create;
  FLock:=TCriticalSection.Create;

  Assert(AParam<>nil);
  FInputPreset:=nil;
  FOutputPreset:=nil;
  FInput:=nil;
  FOutput:=nil;
  FCurrentValue:=DEFAULTPOINTER;
  FVisualisation:=AVisualisation;
  AParam.AttachInterface(Self);
  AParam.AddListener(@LPointerChanged, Self, nil);

  FLock.Enter;
  Update(AParam);
  FLock.Leave;
end;

destructor TLPointerInfo.Destroy;
begin
  FLock.Enter;
  Disconnect;
  FInputPreset:=nil;
  FOutputPreset:=nil;
  FLock.Leave;

  FLock.Destroy;
  inherited Destroy;
end;

procedure TLPointerInfo.DoConnect;
begin
  if (FOutput<>nil) and (FInput<>nil) then begin
    //input is automatically set to output value in TAttachSimpleCommand.SyncNewItem
    FOutput.Attach(FInput, TPPARAMMAPPING, FVisualisation.ID, FCurrentValue.InversePriority);
  end;
end;

procedure TLPointerInfo.DoDisconnect;
begin
  if (FOutput<>nil) and (FInput<>nil)
    then FOutput.Detach(FInput);
end;

procedure TLPointerInfo.UpdateInput;
begin
  if not FCurrentValue.Input.Param.Name.Equals(EmptyString) then begin
    FInputVis:=PresetUtil[FInputPreset.ExecutedValue];
    FInput:=FInputVis[FCurrentValue.Input.Param];
  end else begin
    FInputVis:=nil;
    FInput:=nil;
  end;
end;

procedure TLPointerInfo.UpdateOutput;
begin
  if not FCurrentValue.Output.Param.Name.Equals(EmptyString) then begin
    FOutputVis:=PresetUtil[FOutputPreset.ExecutedValue];
    FOutput:=FOutputVis[FCurrentValue.Output.Param];
  end else begin
    FOutputVis:=nil;
    FOutput:=nil;
  end;
end;

procedure TLPointerInfo.InitInputInPreset;
begin
  FInputPreset:=IPPreset(FVisualisation.Inputs[FCurrentValue.Input.Preset]);
  FInputPreset.AddExecutedListener(@LInputPresetChanged, Self, nil);
  UpdateInput;
end;

procedure TLPointerInfo.InitOutputInPreset;
begin
  FOutputPreset:=IPPreset(FVisualisation.Inputs[FCurrentValue.Output.Preset]);
  FOutputPreset.AddExecutedListener(@LOutputPresetChanged, Self, nil);
  UpdateOutput;
end;

procedure TLPointerInfo.InitInputSingle; inline;
begin
  FInput:=FVisualisation[FCurrentValue.Input.Param];
end;

procedure TLPointerInfo.InitOutputSingle; inline;
begin
  FOutput:=FVisualisation[FCurrentValue.Output.Param];
end;

procedure TLPointerInfo.DisconnectOutput(const AValue: TVPointer);
begin
  if FOutputPreset<>nil then begin
    FOutputPreset.RemoveExecutedListener(@LOutputPresetChanged, Self);
    FOutputPreset:=nil;
  end;
  if FInputPreset<>nil then begin
    FInputPreset.RemoveExecutedListener(@LInputPresetChanged, Self);
    FInputPreset:=nil;
  end;

  DoDisconnect;
  FInput:=nil;
  FOutput:=nil;
  FOutputVis:=nil;
  FInputVis:=nil;
end;

procedure TLPointerInfo.Connect;
begin
  case PointerConnectionMode of
    ptrNormal: begin
        InitOutputInPreset;
        InitInputInPreset;
        DoConnect;
      end;
    ptrInput : begin
        InitOutputSingle;
        InitInputInPreset;
        DoConnect;
      end;
    ptrOutput: begin
        InitOutputInPreset;
        InitInputSingle;
        DoConnect;
      end;
    ptrBridge: begin
        InitOutputSingle;
        InitInputSingle;
        DoConnect;
      end;
    ptrNone  : {do nothing};
    else raise ENotImplemented.Create('Chosen pointer connection mode not implemented.');
  end;
end;

procedure TLPointerInfo.Disconnect;
begin
  DisconnectOutput(FCurrentValue);
end;

procedure TLPointerInfo.Update(AParam: IPPointer);
begin
  if FCurrentValue<>AParam.Value then begin
    Disconnect;
    FCurrentValue:=AParam.Value;
    Connect;
  end;
end;

function TLPointerInfo.PointerConnectionMode: TVPointerConnectionMode;

  function CheckPreset(ASide: TPointerSide): Boolean; inline;
  begin
    with ASide.Preset
      do Result:=Name.Equals(EmptyString) or (&Type = TPParamType(vPreset));
  end;

begin
  if CheckPreset(FCurrentValue.Input) and CheckPreset(FCurrentValue.Output)
    then Result:=ConnectionMode(FCurrentValue)
    else Result:=ptrNone;
end;

{%ENDREGION}
{%REGION TPointerVisualisation}

procedure TPointerVisualisation.GotInput(Input: IPParam);
begin
  if Input.ID.&Type = TPParamType(vPointer)
    then TLPointerInfo.Create(IPPointer(Input), Self);
  inherited GotInput(Input);
end;

{%ENDREGION}

initialization
  DefaultEvents:=TVisualisationEvents.Create(nil, false);
finalization
  DefaultEvents:=nil;
end.

