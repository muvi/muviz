unit TVSPPermissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClassBasic, HashMap, MapKeys, GUIDop, Enumerators,
  TVSPType;

type
  TTVSPGrantEvent                = procedure (AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem) of object;
  TTVSPPermissionValue           = class;
  TTVSPPermissions               = class;

  TTVSPPermissionAttachmentKey   = class
  strict private
    FContent: TObjectItem;
  public
    constructor Create(AContent: TObjectItem);
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
  end;

  TTVSPPermissionAttachmentValue = class (TObjectContainer)
  strict private
    FOwner: TTVSPPermissionValue;
  protected
    procedure ObjectDestroyed; override;
  public
    constructor Create(AOwner: TTVSPPermissionValue; AContent: TObjectItem);
    destructor Destroy; override;
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
  end;

  TTVSPPermissionKey       = class
  strict private
    FHashCode  : PtrInt;
    FPermission: TTVSPPermission;
    FGroup     : TGUID;
  public
    constructor Create(APermission: TTVSPPermission; AGroup: TGUID);
    destructor Destroy; override;
    function Equals(Obj: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
    property Group: TGUID read FGroup;
    property Permission: TTVSPPermission read FPermission;
  end;

  TTVSPPermissionValue     = class (TTVSPPermissionKey)
  strict private
    FOwner          : TTVSPPermissions;
    FLock           : TMultiReadExclusiveWriteSynchronizer;
    FRequesters     : THashMap;
    FPermissionOwner: TTVSPPermissionAttachmentValue;
    //revokes the permission without calling the revoke event or destroying the
    //revoked item
    function RevokeSilent(AObject: TObjectItem; out ANeedsDestruction, AHadPermission: Boolean): TTVSPPermissionAttachmentValue;
  protected
    procedure ObjectDestroyed(AObject: TTVSPPermissionAttachmentValue);
  public
    constructor Create(AOwner: TTVSPPermissions; APermission: TTVSPPermission; AGroup: TGUID; APermissionOwner: TObjectItem);
    destructor Destroy; override;
    //the result indicates, if the permission was not requested before.
    //This request will always result in the "waiting list", because to go here,
    //another attachment must have requested the permission before
    function Grant(AObject: TObjectItem): Boolean;
    //returns, if the Permisison was granted before
    function Revoke(AObject: TObjectItem): Boolean;
    function HasPermission(AObject: TObjectItem): Boolean;
    function RequestedPermission(AObject: TObjectItem): Boolean;
  end;

  TTVSPPermissions         = class
  strict private
    FLock       : TMultiReadExclusiveWriteSynchronizer;
    FPermissions: THashMap;
    FOnGrant    : TTVSPGrantEvent;
    FOnRevoke   : TTVSPGrantEvent;
    function GetPermission(AGroup: TGUID; APermission: TTVSPPermission): TTVSPPermissionValue;
  protected
    procedure Granted(AGroup: TGUID; APermission: TTVSPPermission; APermissionOwner: TObjectItem);
    procedure Revoked(AGroup: TGUID; APermission: TTVSPPermission; APermissionOwner: TObjectItem);
    procedure ObjectDestroyed(AObject: TTVSPPermissionValue);
  public
    constructor Create;
    destructor Destroy; override;
    function StartIterate: TObject;
    function Iterate(APosition: TObject): TTVSPPermissionValue;
    function Grant(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem): Boolean;
    function Revoke(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem): Boolean;
    property Permissions[AGroup: TGUID; APermission: TTVSPPermission]: TTVSPPermissionValue read GetPermission; default;
    property OnGrant: TTVSPGrantEvent read FOnGrant write FOnGrant;
    property OnRevoke: TTVSPGrantEvent read FOnRevoke write FOnRevoke;
  end;

function HasPermission(APermission: TTVSPPermissionValue; AObject: TObjectItem): Boolean;
function RequestedPermission(APermission: TTVSPPermissionValue; AObject: TObjectItem): Boolean;

implementation

{%REGION TTVSPPermissionAttachmentKey}

constructor TTVSPPermissionAttachmentKey.Create(AContent: TObjectItem);
begin
  inherited Create;
  FContent:=AContent;
end;

function TTVSPPermissionAttachmentKey.Equals(Obj: TObject): Boolean;
begin
  Result:=(Obj <> nil) and (GetHashCode = Obj.GetHashCode);
end;

function TTVSPPermissionAttachmentKey.GetHashCode: PtrInt;
begin
  Result:=PtrInt(FContent);
end;

{%ENDREGION}
{%REGION TTVSPPermissionAttachmentValue}

constructor TTVSPPermissionAttachmentValue.Create(AOwner: TTVSPPermissionValue; AContent: TObjectItem);
begin
  inherited Create(AContent);
  FOwner:=AOwner;
end;

destructor TTVSPPermissionAttachmentValue.Destroy;
begin
  inherited Destroy;
end;

procedure TTVSPPermissionAttachmentValue.ObjectDestroyed;
begin
  FOwner.ObjectDestroyed(Self);
  inherited ObjectDestroyed;
end;

function TTVSPPermissionAttachmentValue.Equals(Obj: TObject): Boolean;
begin
  Result:=(Obj <> nil) and (GetHashCode = Obj.GetHashCode);
end;

function TTVSPPermissionAttachmentValue.GetHashCode: PtrInt;
begin
  Result:=PtrInt(Content);
end;

{%ENDREGION}
{%REGION TTVSPPermissionKey}

constructor TTVSPPermissionKey.Create(APermission: TTVSPPermission; AGroup: TGUID);
begin
  inherited Create;
  FPermission:=APermission;
  FGroup:=AGroup;
  FHashCode:=GetGUIDHash(AGroup) xor APermission;
end;

destructor TTVSPPermissionKey.Destroy;
begin
  inherited Destroy;
end;

function TTVSPPermissionKey.Equals(Obj: TObject): Boolean;
var
  AOther: TTVSPPermissionKey;
begin
  Assert(Obj.InheritsFrom(TTVSPPermissionKey));
  AOther:=TTVSPPermissionKey(Obj);
  Result:=(FGroup = AOther.Group) and (FPermission = AOther.Permission);
end;

function TTVSPPermissionKey.GetHashCode: PtrInt;
begin
  Result:=FHashCode;
end;

{%ENDREGION}
{%REGION TTVSPPermissionValue}

constructor TTVSPPermissionValue.Create(AOwner: TTVSPPermissions; APermission: TTVSPPermission; AGroup: TGUID; APermissionOwner: TObjectItem);
begin
  inherited Create(APermission, AGroup);
  FOwner:=AOwner;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FRequesters:=THashMap.Create;
  FPermissionOwner:=TTVSPPermissionAttachmentValue.Create(Self, APermissionOwner);
  FRequesters.Add(FPermissionOwner, FPermissionOwner);
end;

destructor TTVSPPermissionValue.Destroy;
begin
  Assert(FRequesters.Count = 0);
  if FPermissionOwner <> nil
    then FPermissionOwner.Destroy;
  FRequesters.Clean;
  FRequesters.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TTVSPPermissionValue.ObjectDestroyed(AObject: TTVSPPermissionAttachmentValue);
var
  ANeedsDestruction, AHadPermission: Boolean;
  ARevoked                         : TTVSPPermissionAttachmentValue;
begin
  ARevoked:=RevokeSilent(AObject.Content, ANeedsDestruction, AHadPermission);
  Assert(ARevoked = AObject);
  if ANeedsDestruction then begin
    FOwner.ObjectDestroyed(Self);
    Destroy;
  end;
end;

function TTVSPPermissionValue.RevokeSilent(AObject: TObjectItem; out ANeedsDestruction, AHadPermission: Boolean): TTVSPPermissionAttachmentValue;
var
  AEnumerator: TEnumerator;
  AGrantedTo : TTVSPPermissionAttachmentValue;
  AKey       : TTVSPPermissionAttachmentKey;
begin
  AGrantedTo:=nil;
  ANeedsDestruction:=false;
  AKey:=TTVSPPermissionAttachmentKey.Create(AObject);

  FLock.Beginwrite;

  Result:=FRequesters.Remove(AKey) as TTVSPPermissionAttachmentValue;
  if (Result <> nil) then begin
    AHadPermission:=FPermissionOwner.Content = AObject;
    if AHadPermission then begin
      Assert(FPermissionOwner = Result);
      if FRequesters.Count = 0 then begin
        ANeedsDestruction:=true;
        FPermissionOwner:=nil;
      end else begin
        AEnumerator:=FRequesters.Enumerator;
        AEnumerator.MoveNext;
        Assert(AEnumerator.Current <> nil);
        AGrantedTo:=TTVSPPermissionAttachmentValue(AEnumerator.Current);
        FPermissionOwner:=AGrantedTo;
        AEnumerator.Destroy;
      end;
    end else begin
      Assert(FPermissionOwner <> Result);
      Assert(FRequesters.Count > 0);
    end;
  end else AHadPermission:=false;

  FLock.Endwrite;

  AKey.Destroy;
  if AGrantedTo <> nil
    then FOwner.Granted(Group, Permission, AGrantedTo.Content);
end;

function TTVSPPermissionValue.Grant(AObject: TObjectItem): Boolean;
var
  AValue: TTVSPPermissionAttachmentValue;
  AKey  : TTVSPPermissionAttachmentKey;
begin
  AKey:=TTVSPPermissionAttachmentKey.Create(AObject);

  FLock.Beginwrite;

  Result:=not FRequesters.Contains(AObject);
  if not Result then begin
    FLock.Endwrite;
    exit;
  end;

  AValue:=TTVSPPermissionAttachmentValue.Create(Self, AObject);
  FRequesters.Add(AValue, AValue);

  FLock.Endwrite;

  AKey.Destroy;
end;

function TTVSPPermissionValue.Revoke(AObject: TObjectItem): Boolean;
var
  ARevoked                         : TTVSPPermissionAttachmentValue;
  ANeedsDestruction, AHadPermission: Boolean;
begin
  ARevoked:=RevokeSilent(AObject, ANeedsDestruction, AHadPermission);
  Result:=ARevoked <> nil;
  if Result then begin
    Assert(ARevoked.Content = AObject);
    ARevoked.Destroy;
    if AHadPermission
      then FOwner.Revoked(Group, Permission, AObject);
    if ANeedsDestruction then begin
      FOwner.ObjectDestroyed(Self);
      Destroy;
    end;
  end else Assert(not ANeedsDestruction);
end;

function TTVSPPermissionValue.HasPermission(AObject: TObjectItem): Boolean;
begin
  FLock.Beginread;
  Result:=(FPermissionOwner <> nil) and (FPermissionOwner.Content = AObject);
  FLock.Endread;
end;

function TTVSPPermissionValue.RequestedPermission(AObject: TObjectItem): Boolean;
var
  AKey: TTVSPPermissionAttachmentKey;
begin
  AKey:=TTVSPPermissionAttachmentKey.Create(AObject);

  FLock.Beginread;
  Result:=FRequesters.Contains(AKey);
  FLock.Endread;

  AKey.Destroy;
end;

{%ENDREGION}
{%REGION TTVSPPermissions}

constructor TTVSPPermissions.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FPermissions:=THashMap.Create;
end;

destructor TTVSPPermissions.Destroy;
begin
  FPermissions.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

function TTVSPPermissions.StartIterate: TObject;
begin
  FLock.Beginread;
  Result:=FPermissions.Enumerator;
end;

function TTVSPPermissions.Iterate(APosition: TObject): TTVSPPermissionValue;
var
  AEnumerator: TEnumerator;
begin
  Assert((APosition<>nil) and (APosition is TEnumerator));
  AEnumerator:=TEnumerator(APosition);
  if AEnumerator.MoveNext then begin
    Result:=TTVSPPermissionValue(AEnumerator.Current);
  end else begin
    APosition.Destroy;
    Result:=nil;
    FLock.Endread;
  end;
end;

procedure TTVSPPermissions.Granted(AGroup: TGUID; APermission: TTVSPPermission; APermissionOwner: TObjectItem);
begin
  if Assigned(FOnGrant)
    then FOnGrant(AGroup, APermission, APermissionOwner);
end;

procedure TTVSPPermissions.Revoked(AGroup: TGUID; APermission: TTVSPPermission; APermissionOwner: TObjectItem);
begin
  if Assigned(FOnRevoke)
    then FOnRevoke(AGroup, APermission, APermissionOwner);
end;

procedure TTVSPPermissions.ObjectDestroyed(AObject: TTVSPPermissionValue);
var
  ARemoved: TObject;
begin
  FLock.Beginwrite;

  ARemoved:=FPermissions.Remove(AObject);
  Assert(ARemoved = AObject);

  FLock.Endwrite;
end;

function TTVSPPermissions.GetPermission(AGroup: TGUID; APermission: TTVSPPermission): TTVSPPermissionValue;
var
  AKey: TTVSPPermissionKey;
begin
  AKey:=TTVSPPermissionKey.Create(APermission, AGroup);

  FLock.Beginread;
  Result:=TTVSPPermissionValue(FPermissions[AKey]);
  FLock.Endread;

  AKey.Destroy;
end;

function TTVSPPermissions.Grant(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem): Boolean;
var
  AKey    : TTVSPPermissionKey;
  AValue  : TTVSPPermissionValue;
  AGranted: Boolean;
begin
  AKey:=TTVSPPermissionKey.Create(APermission, AGroup);

  FLock.Beginwrite;

  AValue:=FPermissions[AKey] as TTVSPPermissionValue;
  AGranted:=AValue = nil;
  if AGranted then begin
    AValue:=TTVSPPermissionValue.Create(Self, APermission, AGroup, AObject);
    FPermissions.Add(AValue, AValue);
    Result:=true;
  end else begin
    Result:=AValue.Grant(AObject);
  end;

  FLock.Endwrite;

  AKey.Destroy;
  if AGranted
    then Granted(AGroup, APermission, AObject);
end;

function TTVSPPermissions.Revoke(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem): Boolean;
var
  AKey  : TTVSPPermissionKey;
  AValue: TTVSPPermissionValue;
begin
  AKey:=TTVSPPermissionKey.Create(APermission, AGroup);

  FLock.Beginwrite;

  AValue:=FPermissions[AKey] as TTVSPPermissionValue;
  if AValue <> nil
    then Result:=AValue.Revoke(AObject)
    else Result:=false;
  FLock.Endwrite;

  AKey.Destroy;
end;

{%ENDREGION}
{%REGION Misc}

function HasPermission(APermission: TTVSPPermissionValue; AObject: TObjectItem): Boolean;
begin
  Assert(AObject <> nil);
  Result:=(APermission <> nil) and (APermission.HasPermission(AObject));
end;

function RequestedPermission(APermission: TTVSPPermissionValue; AObject: TObjectItem): Boolean;
begin
  Assert(AObject <> nil);
  Result:=(APermission <> nil) and (APermission.RequestedPermission(AObject));
end;

{%ENDREGION}

end.

