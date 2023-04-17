unit TVSPIndexedValues;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPClientIndexer, AbstractNet, TVSPMsg, VisType2,
  TVSPType, TVSPConst, StdPermissions;

type
  TTVSPNewValueGetter= procedure (out ADest) of object;

  TTVSPIndexedValue  = class (TTVSPIndexedObject)
  strict private
    FNet : TANet;
    function GetMemSize: Cardinal; inline;
    procedure SendChangeIndexed(ANewValue: TTVSPNewValueGetter; APermission: TTVSPClientPermission; APermissionGroup: TGUID);
    procedure SendChangeLong(ANewValue: TTVSPNewValueGetter; APermission: TTVSPClientPermission; APermissionGroup: TGUID);
    procedure SendDeleteIndexed(AIndexer: TTVSPClientIndexer);
    procedure SendDeleteLong;
  strict protected
    function GetGroup: TGUID; virtual; abstract;
    function GetName: string; virtual; abstract;
    function GetType: TPParamType; virtual; abstract;
  public
    procedure InitNet(ANet: TANet);
    procedure ReceiveChange(ANewValue: Pointer); virtual; abstract;
    procedure SendChange(ANewValue: TTVSPNewValueGetter; APermission: TTVSPClientPermission; APermissionGroup: TGUID);
    procedure Delete;
    property Group: TGUID read GetGroup;
    property MemSize: Cardinal read GetMemSize;
    property Name: string read GetName;
    property &Type: TPParamType read GetType;
  end;

implementation

function IsPermissionNoLimit(APermission: TTVSPClientPermission): Boolean; inline;
begin
  Result:=APermission > High(TTVSPPermission);
end;

function IsPermissionNoSend(APermission: TTVSPClientPermission): Boolean; inline;
begin
  Result:=APermission < Low(TTVSPPermission);
end;

{%REGION TTVSPIndexedValue}

procedure TTVSPIndexedValue.InitNet(ANet: TANet);
begin
  FNet:=ANet;
end;

function TTVSPIndexedValue.GetMemSize: Cardinal; inline;
begin
  Result:=TVSPParamMemSize(&Type);
end;

procedure TTVSPIndexedValue.Delete;
var
  AIndexer: TTVSPClientIndexer;
begin
  AIndexer:=Indexer;
  if AIndexer<>nil
    then SendDeleteIndexed(AIndexer)
    else SendDeleteLong;
end;

procedure TTVSPIndexedValue.SendDeleteIndexed(AIndexer: TTVSPClientIndexer);
var
  AMsg: TCMDeleteValIndexed;
begin
  Assert(FNet <> nil);

  AMsg:=MakeMsgDeleteValIndexed(Index);
  FNet.Send(nil, @AMsg, SizeOf(AMsg));

  AIndexer.RemoveIndexed(Self);
end;

procedure TTVSPIndexedValue.SendDeleteLong;
var
  AMsg : PCMDeleteVal;
  ASize: Cardinal;
begin
  Assert(FNet <> nil);

  ASize:=MakeMsgDeleteVal(Group, &Type, Name, AMsg);
  FNet.Send(nil, AMsg, ASize);
  FreeMem(AMsg, ASize);
end;

procedure TTVSPIndexedValue.SendChange(ANewValue: TTVSPNewValueGetter; APermission: TTVSPClientPermission; APermissionGroup: TGUID);
begin
  Assert(FNet <> nil);
  Assert(Assigned(ANewValue));
  if IsPermissionNoSend(APermission)
    then exit;
  if Indexed
    then SendChangeIndexed(ANewValue, APermission, APermissionGroup)
    else SendChangeLong(ANewValue, APermission, APermissionGroup);
end;

procedure TTVSPIndexedValue.SendChangeIndexed(ANewValue: TTVSPNewValueGetter; APermission: TTVSPClientPermission; APermissionGroup: TGUID);
var
  AMsg                 : ^TCMValIndexed;
  ASize, ANormalMsgSize: Integer;
  AIsPermissionNoLimit : Boolean;
  APermissionMsg       : PCMPermission;
begin
  AIsPermissionNoLimit:=IsPermissionNoLimit(APermission);

  ANormalMsgSize:=TCMVALINDEXED_HEADERSIZE + MemSize;
  if AIsPermissionNoLimit
    then ASize:=ANormalMsgSize
    else ASize:=ANormalMsgSize + SizeOf(TCMPermission);
  GetMem(AMsg, ASize);
  with AMsg^ do begin
    if AIsPermissionNoLimit
      then MsgID:=tcVALINDEXED
      else MsgID:=tcVALINDEXEDPERMITTED;
    Index:=Self.Index;
    ANewValue(Value);
  end;

  if not AIsPermissionNoLimit then begin
    APermissionMsg:=PCMPermission(Pointer(AMsg) + ANormalMsgSize);
    with APermissionMsg^ do begin
      Group:=APermissionGroup;
      Permission:=APermission;
    end;
  end;

  FNet.Send(nil, AMsg, ASize);
  FreeMem(AMsg, ASize);
end;

procedure TTVSPIndexedValue.SendChangeLong(ANewValue: TTVSPNewValueGetter; APermission: TTVSPClientPermission; APermissionGroup: TGUID);
var
  AMsgPtr              : Pointer;
  AMsg                 : ^TCMVal;
  ASize, ANormalMsgSize: Integer;
  APermissionMsg       : PCMPermission;
  AIsPermissionNoLimit : Boolean;
begin
  AIsPermissionNoLimit:=IsPermissionNoLimit(APermission);

  ANormalMsgSize:=TCMVAL_HEADERSIZE + Length(Name) + MemSize;
  if AIsPermissionNoLimit
    then ASize:=ANormalMsgSize
    else ASize:=ANormalMsgSize + SizeOf(TCMPermission);

  GetMem(AMsgPtr, ASize);
  AMsg:=AMsgPtr;
  with AMsg^ do begin
    if AIsPermissionNoLimit
      then MsgID:=tcVAL
      else MsgID:=tcVALPERMITTED;
    ValueID.GroupID:=Group;
    ValueID.&Type:=&Type;
    ToTVSPString(Name, ValueID.Name);
  end;
  //add value
  ANewValue((AMsgPtr + ASize - MemSize)^);

  if not AIsPermissionNoLimit then begin
    APermissionMsg:=PCMPermission(Pointer(AMsg) + ANormalMsgSize);
    with APermissionMsg^ do begin
      Group:=APermissionGroup;
      Permission:=APermission;
    end;
  end;

  FNet.Send(nil, AMsg, ASize);
  FreeMem(AMsgPtr, ASize);
end;

{%ENDREGION}

end.

