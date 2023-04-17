unit ParamMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csl, HashMap, VisType2, SyncObjs, ParamType2, MapKeys,
  MStrings, StdParamTypes, Enumerators, AttachedParams, ParamNotificationList,
  AttachedInterfaces, MExceptions, TVSPIndexedValues, GlobalTVSP, TVSPType,
  TVSPSources, LogUnit, DoorUnit, Doors, StdPermissions, TVSPClientIndexer;

type
  TVParamMap      = class;
  TVParamValue    = class;

  TVParamKey      = class (TAttachObject)
  strict private
    FID           : TPParamID;
    function GetID: TPParamID; cdecl;
  public
    constructor Create(AID: TPParamID);
    function GetHashCode: PtrInt; override;
    function Equals(AObject: TObject): Boolean; override;
    property ID: TPParamID read FID;
  end;

  TVParamTVSPData = class (TTVSPIndexedValue)
  strict private
    FOwner      : TVParamValue;
    FOwnerBackup: IPParam;
  strict protected
    function GetGroup: TGUID; override;
    function GetName: string; override;
    function GetType: TPParamType; override;
  protected
    procedure InitIndex(AIndexer: TTVSPClientIndexer; AIndex: TTVSPIndex); override;
  public
    constructor Create(AOwner: TVParamValue);
    procedure ReceiveChange(ANewValue: Pointer); override;
    //call to protect the param from beeing destroyed
    procedure Needed; inline;
    procedure RemoveIndex; override;
  end;

  TVParamValue    = class (TVParamKey, IPParamPrototype)
  strict private
    FParamSettings        : IPParamSettings;
    FParam                : IPParam;
    //makes sure that the params refcount is at least 1 if the param has a
    //non-default value
    FNonDefaultBackupParam: IPParam;
    FIsDeleted            : Cardinal;

    FNotifications        : TNotificationList;
    FAttachedInterfaces   : TInterfaces;
    FDoorway              : TDoorway;

    FTVSPData             : TVParamTVSPData;
    function GetAttachedInterface(AID: TGUID): IInterface; cdecl;
    //to enable SyncAttached
    function GetParam: IPParam; override;
    procedure GetParamStream(out ADest);
    function GetOwner: IPVisualisation; cdecl;
    function IsDeleted: Boolean; cdecl;
    //returns, if it was deleted previously
    function SetDeleted(AValue: Boolean): Boolean;
  private
    FOwner                : TVParamMap;
    procedure Unown;
  strict private
    //do not use directly
    constructor Create(AOwner: TVParamMap; AID: TPParamID);
  protected
    procedure ValueChanged(AChangedValue: IChangedValue; AEntryCode: TEntryCode; APermission: TTVSPClientPermission; APermissionGroup: TGUID); cdecl;
    function CheckEntryCode(var AEntryCode: TEntryCode): Boolean; cdecl;
  public
    class function Create(AOwner: TVParamMap; AID: TPParamID; var AParamDest: IPParam): TVParamValue;
    destructor Destroy; override;
    procedure AttachInterface(AInterface: IInterface); cdecl;
    procedure AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
    procedure RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
    procedure Delete; cdecl;
    property AttachedIntefaces[AID: TGUID]: IInterface read GetAttachedInterface;
    property Deleted: Boolean read IsDeleted;
    property Param: IPParam read FParam;
    property ParamSettings: IPParamSettings read FParamSettings;
    property TVSPData: TVParamTVSPData read FTVSPData;
  end;

  TVTVSPGroupable = class (TInterfacedObject)
  strict protected
    function GetGroup: TGUID; virtual; abstract;
    function GetVisualisation: IPVisualisation; virtual; abstract;
  protected
    function RequestParam(APrototype: IPParamPrototype): IPParamSettings; virtual; abstract;
    procedure InitParam(AParamSettings: IPParamSettings); virtual; abstract;
  public
    property Group: TGUID read GetGroup;
    property Visualisation: IPVisualisation read GetVisualisation;
  end;

  TVParamMap      = class
  strict private
    FMap         : TMap;
    FLock        : TCriticalSection;
    function DoGetItem(AID: TPParamID; var AParamDest: IPParam): TVParamValue;
    function GetParam(AID: TPParamID): IPParam;
    function GetIndexedParam(AID: TPParamID): TTVSPIndexedValue;
  private
    FOwner       : TVTVSPGroupable;
  private
    procedure Removed(AValue: TVParamValue);
  public
    constructor Create(AOwner: TVTVSPGroupable);
    destructor Destroy; override;
    function Iterate(var AEnumerator: TEnumerator): IPParamSettings;
    property IndexedParams[AID: TPParamID]: TTVSPIndexedValue read GetIndexedParam;
    property Params[AID: TPParamID]: IPParam read GetParam; default;
  end;

implementation

{%REGION TVParamKey}

constructor TVParamKey.Create(AID: TPParamID);
begin
  Assert(AID.Name <> nil);
  inherited Create;
  FID:=AID;
end;

function TVParamKey.GetHashCode: PtrInt;
begin
  Result:=GetStringHash(FID.Name) xor FID.&Type;
end;

function TVParamKey.Equals(AObject: TObject): Boolean;
begin
  Result:=AObject.InheritsFrom(TVParamKey) and (TVParamKey(AObject).FID=FID);
end;

function TVParamKey.GetID: TPParamID; cdecl;
begin
  Result:=FID;
end;

{%ENDREGION}
{%REGION TVParamTVSPData}

constructor TVParamTVSPData.Create(AOwner: TVParamValue);
begin
  inherited Create;
  FOwner:=AOwner;
  TVSP.RegisterIndex(Self);
end;

procedure TVParamTVSPData.ReceiveChange(ANewValue: Pointer);
begin
  FOwner.Param.FromStream(ANewValue^, TPNOSENDING, NULLVISID);
end;

procedure TVParamTVSPData.Needed; inline;
begin
  FOwnerBackup:=FOwner.Param;
end;

procedure TVParamTVSPData.InitIndex(AIndexer: TTVSPClientIndexer; AIndex: TTVSPIndex);
begin
  //needed is called before in get indexed param
  Assert(FOwnerBackup = FOwner.Param);
  inherited InitIndex(AIndexer, AIndex);
end;

procedure TVParamTVSPData.RemoveIndex;
begin
  inherited RemoveIndex;
  FOwnerBackup:=nil;
end;

function TVParamTVSPData.GetGroup: TGUID;
begin
  Result:=FOwner.FOwner.FOwner.Group;
end;

function TVParamTVSPData.GetName: string;
begin
  Result:=FOwner.ID.Name;
end;

function TVParamTVSPData.GetType: TPParamType;
begin
  Result:=FOwner.ID.&Type;
end;

{%ENDREGION}
{%REGION TVParamValue}

constructor TVParamValue.Create(AOwner: TVParamMap; AID: TPParamID);
begin
  Assert(AOwner <> nil);
  AOwner.FOwner._AddRef;
  inherited Create(AID);
  FDoorway:=TDoorway.Create;
  FParam:=nil;
  FOwner:=AOwner;
  FNotifications:=TNotificationList.Create;
  FAttachedInterfaces:=TInterfaces.Create;

  FNonDefaultBackupParam:=nil;
  FIsDeleted:=Cardinal(true);

  Move(AOwner.FOwner.RequestParam(Self), FParamSettings, SizeOf(FParamSettings));
  //reference counting is fixed later in the other Create
  FParam:=FParamSettings.Param;
  AOwner.FOwner.InitParam(FParamSettings);
  FTVSPData:=TVParamTVSPData.Create(Self);
end;

class function TVParamValue.Create(AOwner: TVParamMap; AID: TPParamID; var AParamDest: IPParam): TVParamValue;
begin
  Result:=TVParamValue.Create(AOwner, AID);
  AParamDest:=Result.FParam;
  //fix the additional refernce to allow for garbage collection
  AParamDest._Release;
end;

destructor TVParamValue.Destroy;
var
  AOwner: TVParamMap;
begin
  FTVSPData.Destroy;
  AOwner:=FOwner;
  FOwner:=nil;
  //may be unowned
  if AOwner <> nil
    then AOwner.Removed(Self);
  Assert(FNonDefaultBackupParam = nil);
  //do this first in any case!
  FAttachedInterfaces.Destroy;
  FNotifications.Destroy;
  //no reference counting!
  fillchar(FParam, SizeOf(FParam), 0);
  AOwner.FOwner._Release;
  fillchar(FParamSettings, SizeOf(FParamSettings), 0);
  FDoorway.Destroy;
  inherited Destroy;
end;

procedure TVParamValue.Unown;
begin
  Assert(FOwner <> nil);
  FOwner:=nil;
end;

function TVParamValue.GetAttachedInterface(AID: TGUID): IInterface; cdecl;
begin
  Result:=FAttachedInterfaces[AID];
end;

procedure TVParamValue.AddListener(AListener: TPParamNotification; AContext: Pointer; AThread: IPThread); cdecl;
begin
  FNotifications.Add(AListener, AContext, AThread);
end;

procedure TVParamValue.RemoveListener(AListener: TPParamNotification; AContext: Pointer); cdecl;
begin
  if not FNotifications.Remove(AListener, AContext)
    then EInvalidListener.Create('Tried to remove a listener which was never added.');
end;

function TVParamValue.IsDeleted: Boolean; cdecl;
var
  AIsDeleted: Cardinal;
begin
  InterLockedExchange(AIsDeleted, FIsDeleted);
  Result:=Boolean(AIsDeleted);
end;

function TVParamValue.SetDeleted(AValue: Boolean): Boolean;
begin
  if AValue
    then FNonDefaultBackupParam:=nil
    else FNonDefaultBackupParam:=FParam;
  Result:=Boolean(InterLockedExchange(FIsDeleted, Cardinal(AValue)));
end;

procedure TVParamValue.Delete; cdecl;
begin
  if not SetDeleted(true)
    then FTVSPData.Delete;
end;

procedure TVParamValue.AttachInterface(AInterface: IInterface); cdecl;
begin
  FAttachedInterfaces.Add(AInterface);
end;

function TVParamValue.GetParam: IPParam;
begin
  Assert(FParam <> nil);
  Result:=FParam;
end;

procedure TVParamValue.GetParamStream(out ADest);
begin
  FParam.ToStream(ADest);
end;

function TVParamValue.GetOwner: IPVisualisation; cdecl;
begin
  Result:=FOwner.FOwner.Visualisation;
end;

function TVParamValue.CheckEntryCode(var AEntryCode: TEntryCode): Boolean; cdecl;
begin
  if AEntryCode = nil then begin
    AEntryCode:=DoorManager.NewEntryCode;
    if AEntryCode <> nil
      then Result:=FDoorway.Enter(AEntryCode);
    Assert(Result);
  end else Result:=FDoorway.Enter(AEntryCode);
end;

procedure TVParamValue.ValueChanged(AChangedValue: IChangedValue; AEntryCode: TEntryCode; APermission: TTVSPClientPermission; APermissionGroup: TGUID); cdecl;
begin
  Assert(FParam <> nil);
  Assert(FNotifications <> nil);
  //TODO: necessary? : //fix the fact that ParamChanged may be called after the param is destroyed
  FParam._AddRef;

  FNotifications.Notify(FParam, AChangedValue);

  FTVSPData.SendChange(@GetParamStream, APermission, APermissionGroup);

  SyncAttached(AEntryCode);
  {
  if AChangedValue.IsDefault
    then FNonDefaultBackupParam:=nil
    else FNonDefaultBackupParam:=FParam;
  }
  SetDeleted(false);
  //fix the refcount forced above
  FParam._Release;
end;

{%ENDREGION}
{%REGION TVParamMap}

constructor TVParamMap.Create(AOwner: TVTVSPGroupable);
begin
  inherited Create;
  FOwner:=AOwner;
  FMap:=THashMap.Create;
  FLock:=TCriticalSection.Create;
end;

destructor TVParamMap.Destroy;
var
  AItem: TObject;
begin
  for AItem in FMap
    do TVParamValue(AItem).Unown;
  FMap.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TVParamMap.Removed(AValue: TVParamValue);
begin
  FLock.Enter;
  FMap.Remove(AValue);
  FLock.Leave;
end;

function TVParamMap.DoGetItem(AID: TPParamID; var AParamDest: IPParam): TVParamValue;
var
  AKey  : TVParamKey;
  AItem : TObject;
begin
  Assert(AID.Name <> nil);

  AKey:=TVParamKey.Create(AID);
  FLock.Enter;

  AItem:=FMap[AKey];
  if AItem<>nil then begin
    Result:=TVParamValue(AItem);
    AParamDest:=Result.Param;
  end else begin
    Result:=TVParamValue.Create(Self, AID, AParamDest);
    FMap.Add(Result, Result);
    Assert(Result.FOwner = Self);
  end;

  FLock.Leave;
  AKey.Destroy;
end;

function TVParamMap.GetParam(AID: TPParamID): IPParam;
begin
  DoGetItem(AID, Result);
end;

function TVParamMap.GetIndexedParam(AID: TPParamID): TTVSPIndexedValue;
var
  AItem : TVParamValue;
  AParam: IPParam;
begin
  AItem:=DoGetItem(AID, AParam);
  Result:=AItem.TVSPData;
  TVParamTVSPData(Result).Needed;
end;

function TVParamMap.Iterate(var AEnumerator: TEnumerator): IPParamSettings;
begin
  if AEnumerator = nil then begin
    FLock.Enter;
    AEnumerator:=FMap.Enumerator;
  end;
  if AEnumerator.MoveNext then begin
    Result:=TVParamValue(AEnumerator.Current).ParamSettings;
  end else begin
    Result:=nil;
    AEnumerator.Destroy;
    AEnumerator:=nil;
    FLock.Leave;
  end;
end;

{%ENDREGION}

end.

