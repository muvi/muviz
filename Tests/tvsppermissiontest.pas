unit TVSPPermissionTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, TVSPPermissions,
  ObjectClassBasic, TVSPType, GUIDop;

const
  TVSPPERMISSIONTESTITEMCOUNT = 20;

type
  TTVSPPermissionTest     = class;

  TTVSPPermissionTestItem = class (TObjectItem)
  strict private
    FOwner: TTVSPPermissionTest;
    FIndex: Integer;
  public
    constructor Create(AOwner: TTVSPPermissionTest; AIndex: Integer);
    destructor Destroy; override;
    property Index: Integer read FIndex;
  end;

  TGrantedData            = record
    Filled    : Boolean;
    &Object   : TObjectItem;
    Permission: TTVSPPermission;
    Group     : TGUID;
  end;

  TTestPermission         = record
    Group     : TGUID;
    Permission: TTVSPPermission;
  end;

  TTVSPPermissionTest     = class(TTestCase)
  strict private
    FPermissions    : TTVSPPermissions;
    FTestPermissions: array [0..TVSPPERMISSIONTESTITEMCOUNT-1] of TTestPermission;
    FGranted        : TGrantedData;
    FRevoked        : TGrantedData;
    FTearingDown    : Boolean;
    procedure Granted(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem);
    procedure Revoked(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem);
  private
    FItems          : array [0..TVSPPERMISSIONTESTITEMCOUNT-1] of TTVSPPermissionTestItem;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestGrantSingle;
    procedure TestGrant;
    procedure TestGrantRequest;
    procedure TestGrantDouble;
    procedure TestRevokeUngranted;
    procedure TestRevokeRequested;
    procedure TestRevokeGranted;
    procedure TestRevokeSingle;
    procedure TestDestroyItemSingle;
    procedure TestHasInvalidPermission;
    procedure TestHasPermission;
    procedure TestRequestedInvalidPermission;
    procedure TestRequestedPermission;
  end;

implementation

{%REGION TTVSPPermissionTestItem}

constructor TTVSPPermissionTestItem.Create(AOwner: TTVSPPermissionTest; AIndex: Integer);
begin
  inherited Create;
  FOwner:=AOwner;
  FIndex:=AIndex;
end;

destructor TTVSPPermissionTestItem.Destroy;
begin
  Assert(FOwner.FItems[FIndex] = Self);
  FOwner.FItems[FIndex]:=nil;
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TTVSPPermissionTest}

procedure TTVSPPermissionTest.TestHookUp;
begin
  //test set up and tear down
end;

procedure TTVSPPermissionTest.TestGrantSingle;
var
  AResult   : Boolean;
  AGrantedTo: TTVSPPermissionTestItem;
begin
  with FTestPermissions[0] do begin
    FGranted.Filled:=false;
    FRevoked.Filled:=false;
    AGrantedTo:=FItems[0];
    AResult:=FPermissions.Grant(Group, Permission, AGrantedTo);

    AssertSame('the item changed', AGrantedTo, FItems[0]);
    AssertTrue('result wrong', AResult);
    AssertFalse('revoked record filled', FRevoked.Filled);
    AssertTrue('granted record not filled', FGranted.Filled);
    AssertSame('granted to the wrong object', FItems[0], FGranted.&Object);
    AssertEquals('granted the wrong Permission', Permission, FGranted.Permission);
    AssertTrue('granted the wrong group', Group = FGranted.Group);
  end;
end;

procedure TTVSPPermissionTest.TestGrant;
var
  I         : Integer;
  AResult   : Boolean;
  AGrantedTo: TTVSPPermissionTestItem;
begin
  for I:=0 to TVSPPERMISSIONTESTITEMCOUNT - 1 do with FTestPermissions[I] do begin
    FGranted.Filled:=false;
    FRevoked.Filled:=false;
    AGrantedTo:=FItems[I];
    AResult:=FPermissions.Grant(Group, Permission, AGrantedTo);

    AssertSame('the item changed', AGrantedTo, FItems[I]);
    AssertTrue('result wrong', AResult);
    AssertFalse('revoked record filled (' + IntToStr(I) + ')', FRevoked.Filled);
    AssertTrue('granted record not filled (' + IntToStr(I) + ')', FGranted.Filled);
    AssertSame('granted to the wrong object (' + IntToStr(I) + ')', FItems[I], FGranted.&Object);
    AssertEquals('granted the wrong Permission (' + IntToStr(I) + ')', Permission, FGranted.Permission);
    AssertTrue('granted the wrong group (' + IntToStr(I) + ')', Group = FGranted.Group);
  end;
end;

procedure TTVSPPermissionTest.TestGrantRequest;
var
  I, J      : Integer;
  AResult   : Boolean;
  AGrantedTo: TTVSPPermissionTestItem;
begin
  TestGrant;
  for I:=0 to TVSPPERMISSIONTESTITEMCOUNT - 1 do
    for J:=1 to 4
      do with FTestPermissions[(I+J) mod TVSPPERMISSIONTESTITEMCOUNT] do begin
        FGranted.Filled:=false;
        FRevoked.Filled:=false;
        AGrantedTo:=FItems[I];
        AResult:=FPermissions.Grant(Group, Permission, AGrantedTo);

        AssertSame('the item changed', AGrantedTo, FItems[I]);
        AssertTrue('result wrong', AResult);
        AssertFalse('revoked record filled (' + IntToStr(I) + ')', FRevoked.Filled);
        AssertFalse('granted record filled (' + IntToStr(I) + ')', FGranted.Filled);
      end;
end;

procedure TTVSPPermissionTest.TestGrantDouble;
var
  AResult   : Boolean;
  AGrantedTo: TTVSPPermissionTestItem;
begin
  TestGrant;
  with FTestPermissions[0] do begin
    FGranted.Filled:=false;
    FRevoked.Filled:=false;
    AGrantedTo:=FItems[0];
    AResult:=FPermissions.Grant(Group, Permission, AGrantedTo);

    AssertSame('the item changed', AGrantedTo, FItems[0]);
    AssertFalse('result wrong', AResult);
    AssertFalse('revoked record filled', FRevoked.Filled);
    AssertFalse('granted record filled', FGranted.Filled);
  end;
end;

procedure TTVSPPermissionTest.TestRevokeUngranted;
var
  AResult   : Boolean;
  AGrantedTo: TTVSPPermissionTestItem;
begin
  TestGrantRequest;
  with FTestPermissions[5] do begin
    FGranted.Filled:=false;
    FRevoked.Filled:=false;
    AGrantedTo:=FItems[0];
    AResult:=FPermissions.Revoke(Group, Permission, AGrantedTo);

    AssertSame('the item changed', AGrantedTo, FItems[0]);
    AssertFalse('result wrong', AResult);
    AssertFalse('revoked record filled', FRevoked.Filled);
    AssertFalse('granted record filled', FGranted.Filled);
  end;
end;

procedure TTVSPPermissionTest.TestRevokeRequested;
var
  AResult   : Boolean;
  AGrantedTo: TTVSPPermissionTestItem;
begin
  TestGrantRequest;
  with FTestPermissions[4] do begin
    FGranted.Filled:=false;
    FRevoked.Filled:=false;
    AGrantedTo:=FItems[0];
    AResult:=FPermissions.Revoke(Group, Permission, AGrantedTo);

    AssertSame('the item changed', AGrantedTo, FItems[0]);
    AssertTrue('result wrong', AResult);
    AssertFalse('revoked record filled', FRevoked.Filled);
    AssertFalse('granted record filled', FGranted.Filled);
  end;
end;

procedure TTVSPPermissionTest.TestRevokeGranted;
var
  AResult     : Boolean;
  AGrantedTo  : TTVSPPermissionTestItem;
  AObjectIndex: Integer;
begin
  TestGrantRequest;
  with FTestPermissions[0] do begin
    FGranted.Filled:=false;
    FRevoked.Filled:=false;
    AGrantedTo:=FItems[0];
    AResult:=FPermissions.Revoke(Group, Permission, AGrantedTo);

    AssertSame('the item changed', AGrantedTo, FItems[0]);
    AssertTrue('result wrong', AResult);

    AssertTrue('revoked record not filled', FRevoked.Filled);
    AssertSame('revoke the wrong object', FItems[0], FRevoked.&Object);
    AssertEquals('revoked the wrong Permission', Permission, FRevoked.Permission);
    AssertTrue('revoked the wrong group', Group = FRevoked.Group);

    AssertTrue('granted record not filled', FGranted.Filled);
    //may grant to any object which was added in TestGrantRequest
    AObjectIndex:=(TTVSPPermissionTestItem(FGranted.&Object).Index + 4) mod TVSPPERMISSIONTESTITEMCOUNT;
    AssertTrue('granted to the wrong object: ' + IntToStr(TTVSPPermissionTestItem(FGranted.&Object).Index), (AObjectIndex >= 0) and (AObjectIndex <= 3));
    AssertEquals('granted the wrong Permission', Permission, FGranted.Permission);
    AssertTrue('granted the wrong group', Group = FGranted.Group);
  end;
end;

procedure TTVSPPermissionTest.TestRevokeSingle;
var
  AResult   : Boolean;
  AGrantedTo: TTVSPPermissionTestItem;
begin
  TestGrant;
  with FTestPermissions[0] do begin
    FGranted.Filled:=false;
    FRevoked.Filled:=false;
    AGrantedTo:=FItems[0];
    AResult:=FPermissions.Revoke(Group, Permission, AGrantedTo);

    AssertSame('the item changed', AGrantedTo, FItems[0]);
    AssertTrue('result wrong', AResult);
    AssertFalse('granted record filled', FGranted.Filled);

    AssertTrue('revoked record not filled', FRevoked.Filled);
    AssertSame('revoke the wrong object', FItems[0], FRevoked.&Object);
    AssertEquals('revoked the wrong Permission', Permission, FRevoked.Permission);
    AssertTrue('revoked the wrong group', Group = FRevoked.Group);
  end;
end;

procedure TTVSPPermissionTest.TestDestroyItemSingle;
begin
  TestGrant;
  with FTestPermissions[0] do begin
    FGranted.Filled:=false;
    FRevoked.Filled:=false;
    FItems[0].Destroy;

    AssertSame('the item was not destroyed', nil, FItems[0]);
    AssertFalse('granted record filled', FGranted.Filled);
    AssertFalse('revoked record filled', FRevoked.Filled);
  end;
end;

procedure TTVSPPermissionTest.TestHasInvalidPermission;
var
  APermission: TTVSPPermissionValue;
  AGroup     : TGUID;
begin
  TestGrantRequest;
  CreateGUID(AGroup);
  APermission:=FPermissions[AGroup, 1337];
  AssertNull('invalid permission found', APermission);
  AssertFalse('invalid permition was granted', HasPermission(APermission, FItems[0]));
end;

procedure TTVSPPermissionTest.TestRequestedInvalidPermission;
var
  APermission: TTVSPPermissionValue;
  AGroup     : TGUID;
begin
  TestGrantRequest;
  CreateGUID(AGroup);
  APermission:=FPermissions[AGroup, 1337];
  AssertNull('invalid permission found', APermission);
  AssertFalse('invalid permition was requested', RequestedPermission(APermission, FItems[0]));
end;

procedure TTVSPPermissionTest.TestHasPermission;
var
  I, J       : Integer;
  APermission: TTVSPPermissionValue;
begin
  TestGrantRequest;
  for I:=0 to TVSPPERMISSIONTESTITEMCOUNT - 1 do begin
    with FTestPermissions[I] do begin
      APermission:=FPermissions[Group, Permission];
      AssertNotNull('permission is nil', APermission);
      AssertTrue('item does not have the appropriate permition', HasPermission(APermission, FItems[I]));
    end;
    for J:=1 to 4 do with FTestPermissions[(I+J) mod TVSPPERMISSIONTESTITEMCOUNT] do begin
      APermission:=FPermissions[Group, Permission];
      AssertNotNull('permission is nil', APermission);
      AssertFalse('item has an inappropriate permition', HasPermission(APermission, FItems[I]));
    end;
    with FTestPermissions[(I+5) mod TVSPPERMISSIONTESTITEMCOUNT] do begin
      APermission:=FPermissions[Group, Permission];
      AssertNotNull('permission is nil', APermission);
      AssertFalse('item has an inappropriate permition', HasPermission(APermission, FItems[I]));
    end;
  end;
end;

procedure TTVSPPermissionTest.TestRequestedPermission;
var
  I, J       : Integer;
  APermission: TTVSPPermissionValue;
begin
  TestGrantRequest;
  for I:=0 to TVSPPERMISSIONTESTITEMCOUNT - 1 do begin
    with FTestPermissions[I] do begin
      APermission:=FPermissions[Group, Permission];
      AssertNotNull('permission is nil', APermission);
      AssertTrue('item does not have the appropriate permition', RequestedPermission(APermission, FItems[I]));
    end;
    for J:=1 to 4 do with FTestPermissions[(I+J) mod TVSPPERMISSIONTESTITEMCOUNT] do begin
      APermission:=FPermissions[Group, Permission];
      AssertNotNull('permission is nil', APermission);
      AssertTrue('item has an inappropriate permition', RequestedPermission(APermission, FItems[I]));
    end;
    with FTestPermissions[(I+5) mod TVSPPERMISSIONTESTITEMCOUNT] do begin
      APermission:=FPermissions[Group, Permission];
      AssertNotNull('permission is nil', APermission);
      AssertFalse('item has an inappropriate permition', RequestedPermission(APermission, FItems[I]));
    end;
  end;
end;

procedure TTVSPPermissionTest.Granted(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem);
begin
  with FGranted do begin
    if Filled and not FTearingDown
      then Fail('Granted record already filled');
    Filled:=true;
    Group:=AGroup;
    Permission:=APermission;
    &Object:=AObject;
  end;
end;

procedure TTVSPPermissionTest.Revoked(AGroup: TGUID; APermission: TTVSPPermission; AObject: TObjectItem);
begin
  with FRevoked do begin
    if Filled and not FTearingDown
      then Fail('Revoked record already filled');
    Filled:=true;
    Group:=AGroup;
    Permission:=APermission;
    &Object:=AObject;
  end;
end;

procedure TTVSPPermissionTest.SetUp;
var
  I: Integer;
begin
  FTearingDown:=false;
  FPermissions:=TTVSPPermissions.Create;
  FPermissions.OnGrant:=@Granted;
  FPermissions.OnRevoke:=@Revoked;
  FGranted.Filled:=false;
  FRevoked.Filled:=false;
  for I:=0 to TVSPPERMISSIONTESTITEMCOUNT-1 do begin
    FItems[I]:=TTVSPPermissionTestItem.Create(Self, I);
    with FTestPermissions[I] do begin
      CreateGUID(Group);
      Permission:=Random(High(TTVSPPermission));
    end;
  end;
end;

procedure TTVSPPermissionTest.TearDown;
var
  I: Integer;
begin
  FTearingDown:=true;
  for I:=0 to TVSPPERMISSIONTESTITEMCOUNT-1
    do if FItems[I] <> nil
      then FItems[I].Destroy;
  FPermissions.Destroy;
end;

{%EnDREGION}

initialization
  RegisterTest(TTVSPPermissionTest);
end.

