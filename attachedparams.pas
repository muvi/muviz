unit AttachedParams;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, VisType2, Doors, ParamType2, DoorUtils, LogUnit, MStrings,
  StdPermissions, VisDebugUtils;

type
  TAttachedParam = record
    VirtualParam   : IPParam;
    TrueParam      : IPParam;
    InversePriority: LongInt;
    Permission     : TTVSPClientPermission;
    PermissionOwner: TGUID;
  end;

  TAttachObject  = class (TInterfacedObject, IDebugAttachObject)
  protected
    function GetParam: IPParam; virtual; abstract;
  private
    FAttachedLock  : TMultiReadExclusiveWriteSynchronizer;
    FAttachedParams: array of TAttachedParam;
    FDoorway       : IDoorway;
    property Param: IPParam read GetParam;
    function GetAttachedParams: string; experimental;
  protected
    procedure SyncAttached(AEntryCode: TEntryCode);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Attach(AParam: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AInversePriority: LongInt); cdecl;
    procedure Detach(AParam: IPParam); cdecl;
    function LowestInversePriority: LongInt; cdecl;
    function HighestInversePriority: LongInt; cdecl;
  end;

implementation

{%REGION TAttachObject}

constructor TAttachObject.Create;
begin
  inherited Create;
  Assert(DoorManager <> nil);
  FAttachedLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FDoorway:=DoorManager.NewDoorway;
end;

destructor TAttachObject.Destroy;
begin
  Assert(Length(FAttachedParams) = 0);
  FDoorway:=nil;
  FAttachedLock.Destroy;
  inherited Destroy;
end;

function TAttachObject.GetAttachedParams: string;
var
  I: Integer;
begin
  Result:='';

  FAttachedLock.Beginread;

  for I:=0 to Length(FAttachedParams)-1
    do with FAttachedParams[I]
      do Result += TrueParam.ID.Name + ' [' + IntToStr(InversePriority) + ']'#$D#$A;

  FAttachedLock.Endread;
end;

procedure TAttachObject.Attach(AParam: IPParam; APermission: TTVSPClientPermission; APermissionOwner: TGUID; AInversePriority: LongInt); cdecl;
var
  I, L    : Integer;
  ANewItem: TAttachedParam;
begin
  Log('[Attached] ' + GUIDToString(AParam.Owner.ID) + '.' + AParam.ID.Name + ' to ' + GUIDToString(Param.Owner.ID) + '.' + Param.ID.Name);
  Assert(Param <> nil);
  Assert(AParam <> nil);

  //prevent the param from beeing destroyed if there are params attched to it.
  Param._AddRef;

  //make new item
  with ANewItem do begin
    TrueParam:=AParam;
    InversePriority:=AInversePriority;
    Permission:=APermission;
    PermissionOwner:=APermissionOwner;
    if AParam.ID.&Type = Param.ID.&Type
      then VirtualParam:=AParam
      //convert param
      else VirtualParam:=ParamTypeUtil[Param.ID.&Type].Convert(AParam);
  end;

  //insert new item
  FAttachedLock.Beginwrite;

  L:=Length(FAttachedParams);
  SetLength(FAttachedParams, L+1);
  I:=L;
  if I > 0 then while FAttachedParams[I-1].InversePriority > AInversePriority do begin
    FAttachedParams[I]:=FAttachedParams[I-1];
    Dec(I);
    if I <= 0
      then break;
  end;
  FAttachedParams[I]:=ANewItem;

  FAttachedLock.Endwrite;

  //sync new item
  //to fix that during creation some rarely set values may not be set
  //due to the fact that the destination is not yet created
  ANewItem.VirtualParam.GetFrom(Param, APermission, APermissionOwner);
end;

procedure TAttachObject.Detach(AParam: IPParam); cdecl;
var
  I, L: Integer;
begin
  Log('[Detached] ' + GUIDToString(AParam.Owner.ID) + '.' + AParam.ID.Name + ' from ' + GUIDToString(Param.Owner.ID) + '.' + Param.ID.Name);
  FAttachedLock.Beginwrite;

  L:=Length(FAttachedParams)-1;
  Assert(L>=0);
  I:=L;
  while FAttachedParams[I].TrueParam<>AParam do begin
    Dec(I);
    Assert(I>=0);
  end;
  //preserve order
  while I < L do begin
    FAttachedParams[I]:=FAttachedParams[I+1];
    Inc(I);
  end;
  SetLength(FAttachedParams, L);

  FAttachedLock.Endwrite;

  //fix reference count forced in Attach()
  Param._Release;
end;

procedure TAttachObject.SyncAttached(AEntryCode: TEntryCode);
var
  I: Integer;
begin
  FAttachedLock.Beginread;

  for I:=0 to Length(FAttachedParams)-1 do begin
    //Caution: this logging is extremely slow!!!
    //Log('Sync ' + GUIDToString(Param.Owner.ID) + '.' + Param.ID.Name + ' to ' + GUIDToString(FAttachedParams[I].TrueParam.Owner.ID) + '.' + FAttachedParams[i].TrueParam.ID.Name);
    with FAttachedParams[I]
      do VirtualParam.GetFrom(Param, Permission, PermissionOwner, AEntryCode);
  end;

  FAttachedLock.Endread
end;

function TAttachObject.LowestInversePriority: LongInt; cdecl;
begin
  FAttachedLock.BeginRead;

  if Length(FAttachedParams) > 0
    then Result:=FAttachedParams[0].InversePriority
    else Result:=0;

  FAttachedLock.Endread;
end;

function TAttachObject.HighestInversePriority: LongInt; cdecl;
var
  L: Integer;
begin
  FAttachedLock.BeginRead;

  L:=Length(FAttachedParams);
  if L > 0
    then Result:=FAttachedParams[Length(FAttachedParams)-1].InversePriority
    else Result:=0;

  FAttachedLock.Endread;
end;

{%ENDREGION}

end.

