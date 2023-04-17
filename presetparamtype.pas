unit PresetParamType;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ParamTypeImpl, VisType2, ParamType2, MTypes, StdParamTypes,
  StdParamTypeImpl, PresetParamConverters, AdvParamType, PluginType, MStrings,
  FAPParamType, TVSPSources, GlobalVisualisationAliveMap, ParamNotificationList,
  MExceptions, Doors, StdPermissions;

type
  TVChangedPreset       = class (specialize TPCustomChangedValue<TVPreset>, IChangedPreset)
  strict private
    FExecutedValue: TVPreset;
    function GetExecutedValue: TVPreset; cdecl;
  public
    function GetIChangedValue: IChangedValue; override;
    property ExecutedValue: TVPreset read FExecutedValue write FExecutedValue;
  end;

  TVPresetValueStorage  = class (TPParamValueStorage, IPPreset)
  protected
    FValue      : TVPreset;
    FLock       : TMultiReadExclusiveWriteSynchronizer;
    function isForceAlive: Boolean; cdecl; virtual;
    procedure AddExecutedListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl; virtual;
    procedure RemoveExecutedListener(AListener: TPParamNotification; AContext: Pointer); cdecl; virtual;
  public
    constructor Create(APrototype: IPParamPrototype);
    destructor Destroy; override;
    //gets the value from AParam and stores it
    procedure GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    procedure ToStream(out ADest); cdecl; override;
    procedure FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;
    function GetParam: IPParam; cdecl; override;
    procedure &Set(Value: TVPreset); cdecl;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
    function Get: TVPreset; cdecl;
    function GetExecutedValue: TVPreset; cdecl; virtual;
    procedure MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl; override;

    property ExecutedValue: TVPreset read GetExecutedValue;
    property ForceAlive: Boolean read IsForceAlive;
    property Param: IPParam read GetParam;
    property RValue: TVPreset read Get write &Set;
  end;

  TVFAPValueStorage     = class (TVPresetValueStorage, IPForceAlivePreset, IPPresetSettings)
  strict private
    //locked with FLock from TVPresetValueStorage
    FExecutedValue        : TVPreset;

    FEnvironment          : IPVisualisationEnvironment;
    FEnvironmentLock      : TMultiReadExclusiveWriteSynchronizer;

    FExecutedNotifications: TNotificationList;
  protected
    procedure AddExecutedListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl; override;
    procedure RemoveExecutedListener(AListener: TPParamNotification; AContext: Pointer); cdecl; override;
    procedure SetEnvironment(AEnvironment: IPVisualisationEnvironment); cdecl;
    procedure DoSet(AValue: TPChangedValue); override;
    function isForceAlive: Boolean; cdecl; override;
    procedure BufferAll(ATag: IString); cdecl;
    procedure BufferAll; cdecl;
    procedure ReleaseAll; cdecl;
    procedure Buffer(AID: TPPresetID); cdecl;
    procedure Release(AID: TPPresetID); cdecl;
  public
    constructor Create(APrototype: IPParamPrototype);
    destructor Destroy; override;
    function GetExecutedValue: TVPreset; cdecl; override;
  end;

  TVPresetType          = class (TPParamTypeImpl)
  protected
    function GetColor: TMColor; cdecl; override;
    function GetName: IString; cdecl; override;
    function GetPicture: TPParamPic; cdecl; override;
    function GetType: TPParamType; cdecl; override;
  public
    function CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl; override;
    function Convert(ADest: IPParam): IPParam; cdecl; override;
  end;

  TVDummyPresetConverter= class (TVPresetParam)
  private
     FConvertFrom: IPParam;
  public
    constructor Create(AConvertFrom: IPParam);
    destructor Destroy; override;
    procedure &Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl; override;
    function Get: TVPreset; cdecl; override;
  end;

procedure Register;

implementation

const
  APicStr: string = #0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#250#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#130#0#130#0#0#0#130#0#130#0#130#0#0#0#130#0#130#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#130#0#130#0#130#0#0#0#130#0#130#0#130#0#0#0#130#0#130#0#130#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0#0;

{%REGION TVChangedPreset}

function TVChangedPreset.GetExecutedValue: TVPreset; cdecl;
begin
  Result:=FExecutedValue;
end;

function TVChangedPreset.GetIChangedValue: IChangedValue;
begin
  Result:=IChangedPreset(Self);
end;

{%ENDREGION}
{%REGION TVPresetValueStorage}

constructor TVPresetValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  FValue:=NULLPRESETID;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TVPresetValueStorage.Destroy;
begin
  FLock.Destroy;
  inherited Destroy;
end;

procedure TVPresetValueStorage.AddExecutedListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
begin
  AddListener(AListener, AContext, AThread);
end;

procedure TVPresetValueStorage.RemoveExecutedListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  RemoveListener(AListener, AContext);
end;

function TVPresetValueStorage.isForceAlive: Boolean; cdecl;
begin
  Result:=false;
end;

procedure TVPresetValueStorage.GetFrom(ASrc: IPParam; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  Assert(ASrc.ID.&Type = TPParamType(vPreset));
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedPreset.Create(IPPreset(ASrc).Get, DEFAULTPRESET), APermission, APermissionGroup, AEntryCode);
end;

procedure TVPresetValueStorage.ToStream(out ADest); cdecl;
begin
  FLock.Beginread;
  TVPreset(ADest):=FValue;
  FLock.Endread;
end;

procedure TVPresetValueStorage.FromStream(const ASrc; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  RegisterChange(TVChangedPreset.Create(TVPreset(ASrc), DEFAULTPRESET), APermission, APermissionGroup, AEntryCode);
end;

function TVPresetValueStorage.GetParam: IPParam; cdecl;
begin
  Result:=IPPreset(Self);
end;

procedure TVPresetValueStorage.&Set(Value: TVPreset); cdecl;
begin
  &Set(Value, TPNOLIMIT, NULLVISID, nil);
end;

procedure TVPresetValueStorage.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  if Prototype.CheckEntryCode(AEntryCode)
    then RegisterChange(TVChangedPreset.Create(Value, DEFAULTPRESET), APermission, APermissionGroup, AEntryCode);
end;

function TVPresetValueStorage.Get: TVPreset; cdecl;
begin
  FLock.Beginread;
  Result:=FValue;
  FLock.Endread;
end;

function TVPresetValueStorage.GetExecutedValue: TVPreset; cdecl;
begin
  Result:=Get;
end;

procedure TVPresetValueStorage.MakeDefault(APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode = nil); cdecl;
begin
  &Set(DEFAULTPRESET, APermission, APermissionGroup, AEntryCode);
end;

{%ENDREGION}
{%REGION TVFAPValueStorage}

constructor TVFAPValueStorage.Create(APrototype: IPParamPrototype);
begin
  inherited Create(APrototype);
  FExecutedNotifications:=TNotificationList.Create;
  FExecutedValue:=FValue;
  FEnvironment:=nil;
  FEnvironmentLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TVFAPValueStorage.Destroy;
begin
  FEnvironmentLock.Destroy;
  FExecutedNotifications.Destroy;
  inherited Destroy;
end;

procedure TVFAPValueStorage.AddExecutedListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
begin
  FExecutedNotifications.Add(AListener, AContext, AThread);
end;

procedure TVFAPValueStorage.RemoveExecutedListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  if not FExecutedNotifications.Remove(AListener, AContext)
    then EInvalidListener.Create('Tried to remove a listener which was never added.');
end;

procedure TVFAPValueStorage.SetEnvironment(AEnvironment: IPVisualisationEnvironment); cdecl;
var
  AForceAliveChanged: Boolean;
  AChangedValue     : TVChangedPreset;
begin
  FEnvironmentLock.Beginwrite;
  AForceAliveChanged:=(FEnvironment = nil) xor (AEnvironment = nil);
  FEnvironment:=AEnvironment;
  FEnvironmentLock.Endwrite;
  //update value
  if AForceAliveChanged then begin
    //problem: this forces sending the value over network, even if the environment is set while creation
    //RValue:=RValue;
    //this won't notify pointers, so they are not changed accordingly

    //do NOT call the params' main listener in any obscure way. This will make
    //the param trying to send updates over the network before it is completely
    //initialized (the environment may be set in GetInput, but the value NOT)
    FLock.Beginwrite;

    if AEnvironment <> nil
      then FExecutedValue:=GlobalVisualisationAliveMap.Load(Prototype.Owner.ID, Prototype.ID.Name, FValue, AEnvironment).ID
      else FExecutedValue:=FValue;
    AChangedValue:=TVChangedPreset.Create(FValue, DEFAULTPRESET);
    AChangedValue.ExecutedValue:=FExecutedValue;

    FLock.Endwrite;

    //notify ONLY the executed notifications
    FExecutedNotifications.Notify(Param, AChangedValue.GetIChangedValue);
  end;
end;

procedure TVFAPValueStorage.DoSet(AValue: TPChangedValue);
var
  ANewValue: TVChangedPreset;
begin
  ANewValue:=TVChangedPreset(AValue);

  FEnvironmentLock.Beginread;
  if FEnvironment <> nil
    then ANewValue.ExecutedValue:=GlobalVisualisationAliveMap.Load(Prototype.Owner.ID, Prototype.ID.Name, ANewValue.Value, FEnvironment).ID
    else ANewValue.ExecutedValue:=ANewValue.Value;
  FEnvironmentLock.Endread;

  FLock.Beginwrite;
  FValue:=ANewValue.Value;
  FExecutedValue:=ANewValue.ExecutedValue;
  FLock.Endwrite;

  inherited DoSet(AValue);

  //notify the executed notifications, too
  FExecutedNotifications.Notify(Param, AValue.GetIChangedValue);
end;

function TVFAPValueStorage.IsForceAlive: Boolean; cdecl;
begin
  FEnvironmentLock.Beginread;
  Result:=FEnvironment <> nil;
  FEnvironmentLock.Endread;
end;

function TVFAPValueStorage.GetExecutedValue: TVPreset; cdecl;
begin
  FLock.Beginread;
  Result:=FExecutedValue;
  FLock.Endread;
end;

procedure TVFAPValueStorage.BufferAll(ATag: IString); cdecl;
begin
  FEnvironmentLock.Beginread;
  GlobalVisualisationAliveMap.BufferAll(Prototype.Owner.ID, Prototype.ID.Name, ATag, FEnvironment);
  FEnvironmentLock.Endread;
end;

procedure TVFAPValueStorage.BufferAll; cdecl;
begin
  FEnvironmentLock.Beginread;
  GlobalVisualisationAliveMap.BufferAll(Prototype.Owner.ID, Prototype.ID.Name, FEnvironment);
  FEnvironmentLock.Endread;
end;

procedure TVFAPValueStorage.ReleaseAll; cdecl;
begin
  GlobalVisualisationAliveMap.ReleaseAll(Prototype.Owner.ID, Prototype.ID.Name);
end;

procedure TVFAPValueStorage.Buffer(AID: TPPresetID); cdecl;
begin
  FEnvironmentLock.Beginread;
  GlobalVisualisationAliveMap.Buffer(Prototype.Owner.ID, Prototype.ID.Name, AID, FEnvironment);
  FEnvironmentLock.Endread;
end;

procedure TVFAPValueStorage.Release(AID: TPPresetID); cdecl;
begin
  GlobalVisualisationAliveMap.Release(Prototype.Owner.ID, Prototype.ID.Name, AID);
end;

{%ENDREGION}
{%REGION TVPresetType}

function TVPresetType.GetColor: TMColor; cdecl;
begin
  Result:=$FFFFC000;
end;

function TVPresetType.GetName: IString; cdecl;
begin
  Result:='Preset';
end;

function TVPresetType.GetPicture: TPParamPic; cdecl;
begin
  Result:=APicStr;
end;

function TVPresetType.GetType: TPParamType; cdecl;
begin
  Result:=vPreset;
end;

function TVPresetType.CreateValueStorage(APrototype: IPParamPrototype): IPParamSettings; cdecl;
begin
  Result:=IPPresetSettings(TVFAPValueStorage.Create(APrototype));
end;

function TVPresetType.Convert(ADest: IPParam): IPParam; cdecl;
begin
  case ADest.ID.&Type of
    vCall        : Result:=IPPreset(TVCallToPreset.Create(IPCall(ADest)));
    vInteger     : Result:=IPPreset(TVIntegerToPreset.Create(IPInteger(ADest)));
    vFloat       : Result:=IPPreset(TVFloatToPreset.Create(IPFloat(ADest)));
    vColor       : Result:=IPPreset(TVColorToPreset.Create(IPColor(ADest)));
    vBoolean     : Result:=IPPreset(TVBooleanToPreset.Create(IPBoolean(ADest)));
    vBuffer      : Result:=IPPreset(TVBufferToPreset.Create(IPBuffer(ADest)));
    vString      : Result:=IPPreset(TVStringToPreset.Create(IPString(ADest)));
    vPreset      : Result:=ADest;
    vPointer     : Result:=IPPreset(TVPointerToPreset.Create(IPPointer(ADest)));
    else           Result:=IPPreset(TVDummyPresetConverter.Create(ADest));
  end;
end;

{%ENDREGION}
{%REGION TDummyPresetConverter}

constructor TVDummyPresetConverter.Create(AConvertFrom: IPParam);
begin
  inherited Create(AConvertFrom);
  FConvertFrom:=AConvertFrom;
end;

destructor TVDummyPresetConverter.Destroy;
begin
  FConvertFrom:=nil;
end;

procedure TVDummyPresetConverter.&Set(Value: TVPreset; APermission: TTVSPClientPermission; APermissionGroup: TGUID; AEntryCode: TEntryCode); cdecl;
begin
  FConvertFrom.GetFrom(FConvertFrom, APermission, APermissionGroup, AEntryCode);
end;

function TVDummyPresetConverter.Get: TVPreset; cdecl;
begin
  Result:=DEFAULTPRESET;
end;

{%ENDREGION}
{%REGION General}

procedure Register;
begin
  GlobalVisualisationAliveMap.Register;
  ParamTypeUtil.AddType(TVPresetType.Create);
end;

{%ENDREGION}

end.

