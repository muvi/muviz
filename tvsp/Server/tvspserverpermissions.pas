unit TVSPServerPermissions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPPermissions, TVSPServerAttachment, TVSPServerError,
  TVSPMsg, TVSPType, ObjectClassBasic;

type
  TTVSPServerPermissions = class
  strict private
    FPermissions: TTVSPPermissions;
    function GetPermission(AGroup: TGUID; APermission: TTVSPPermission): TTVSPPermissionValue;
    procedure Granted(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem);
    procedure Revoked(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem);
  public
    constructor Create;
    destructor Destroy; override;
    function StartIterate: TObject;
    function Iterate(APosition: TObject): TTVSPPermissionValue;
    procedure Grant(AGroup: TGUID; APermission: TTVSPPermission; AAttachment: TTVSPServerAttachment);
    procedure Revoke(AGroup: TGUID; APermission: TTVSPPermission; AAttachment: TTVSPServerAttachment);
    property Permissions[AGroup: TGUID; APermission: TTVSPPermission]: TTVSPPermissionValue read GetPermission; default;
  end;

function Permissions: TTVSPServerPermissions;

implementation

{%REGION TTVSPServerPermissions}

constructor TTVSPServerPermissions.Create;
begin
  inherited Create;
  FPermissions:=TTVSPPermissions.Create;
  FPermissions.OnGrant:=@Granted;
  FPermissions.OnRevoke:=@Revoked;
end;

destructor TTVSPServerPermissions.Destroy;
begin
  FPermissions.Destroy;
  inherited Destroy;
end;

function TTVSPServerPermissions.StartIterate: TObject;
begin
  Result:=FPermissions.StartIterate;
end;

function TTVSPServerPermissions.Iterate(APosition: TObject): TTVSPPermissionValue;
begin
  Result:=FPermissions.Iterate(APosition);
end;

procedure TTVSPServerPermissions.Granted(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem);
var
  AMsg: TSMGrant;
begin
  AMsg:=MakeMsgGrant(AGroup, APermission);
  TTVSPServerAttachment(AObject).Send(@AMsg, SizeOf(AMsg));
end;

procedure TTVSPServerPermissions.Revoked(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem);
begin
  //this should not happen right now...
  Assert(false);
end;

function TTVSPServerPermissions.GetPermission(AGroup: TGUID; APermission: TTVSPPermission): TTVSPPermissionValue;
begin
  Result:=FPermissions.Permissions[AGroup, APermission];
end;

procedure TTVSPServerPermissions.Grant(AGroup: TGUID; APermission: TTVSPPermission; AAttachment: TTVSPServerAttachment);
begin
  if not FPermissions.Grant(AGroup, APermission, AAttachment)
    then raise ETVSPServerError.Create(teALREADYGRANTED, IntToStr(APermission) + ' of ' + GUIDToString(AGroup));
end;

procedure TTVSPServerPermissions.Revoke(AGroup: TGUID; APermission: TTVSPPermission; AAttachment: TTVSPServerAttachment);
begin
  if not FPermissions.Revoke(AGroup, APermission, AAttachment)
    then raise ETVSPServerError.Create(teNOTGRANTED, IntToStr(APermission) + ' of ' + GUIDToString(AGroup));
end;

{%ENDREGION}
{%REGION Misc}

var
  LPermissions: TTVSPServerPermissions = nil;

function Permissions: TTVSPServerPermissions;
begin
  Assert(LPermissions <> nil);
  Result:=LPermissions;
end;

{%ENDREGION}

initialization
  LPermissions:=TTVSPServerPermissions.Create;
finalization
  LPermissions.Destroy;
end.

