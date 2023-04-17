unit TVSPParamGroupMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, TVSPGroups, LinkedList, VisType2, Enumerators,
  TVSPServerAttachment, LockedObjects, TVSPMsg, ObjectClasses, TVSPType,
  TVSPConst;

type
  TTVSPParamGroupedValue = class (TTVSPIndexedValue)
  private
    FParamGroupMapPos: TEnumerator;
  public
    constructor Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
    destructor Destroy; override;
  end;

  TTVSPParamGroupMapEntry= class (TTVSPBasicValue)
  strict private
    FLock       : TMultiReadExclusiveWriteSynchronizer;
    FList       : TLinkedList;
    FAttachments: TLockedObjects;
    procedure SendMsgValGroup(AMsgID: TTVSPMsgID; AGroup: TGUID);
  private
    function Add(AValue: TTVSPParamGroupedValue): TEnumerator;
    procedure Remove(AValue: TTVSPParamGroupedValue);
  public
    constructor Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
    destructor Destroy; override;
    function Subscribe(AAttachment: TTVSPServerAttachment): Boolean;
  end;

  TTVSPParamGroupMap     = class (TTVSPBasicGroup)
  strict private
    function GetItem(AName: string; AType: TPParamType): TTVSPParamGroupMapEntry;
  strict protected
    function CreateValue(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType): TTVSPBasicValue; override;
  public
    constructor Create;
    property Items[AName: string; AType: TPParamType]: TTVSPParamGroupMapEntry read GetItem; default;
  end;

function ParamGroupMap: TTVSPParamGroupMap;
procedure Init;
procedure Done;

implementation

{%REGION TTVSPParamGroupedValue}

constructor TTVSPParamGroupedValue.Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
begin
  inherited Create(AOwner, AName, AType);
  FParamGroupMapPos:=ParamGroupMap[AName, AType].Add(Self);
end;

destructor TTVSPParamGroupedValue.Destroy;
begin
  ParamGroupMap[Key, &Type].Remove(Self);
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TTVSPParamGroupMapEntry}

constructor TTVSPParamGroupMapEntry.Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
begin
  inherited Create(AOwner, AName, AType);
  FAttachments:=TLockedObjects.Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FList:=TLinkedList.Create;
end;

destructor TTVSPParamGroupMapEntry.Destroy;
begin
  FLock.Destroy;
  FList.Destroy;
  FAttachments.Destroy;
  inherited Destroy;
end;

procedure TTVSPParamGroupMapEntry.SendMsgValGroup(AMsgID: TTVSPMsgID; AGroup: TGUID);
var
  AItem: TObjectListItem;
  AMsg : PSMValGroup;
  ASize: Cardinal;
begin
  ASize:=MakeMsgValGroup(AMsgID, AGroup, &Type, Key, AMsg);

  AItem:=nil;
  FAttachments.IterateObjects(AItem);
  while AItem<>nil do begin
    TTVSPServerAttachment(AItem.Content).Send(AMsg, ASize);
    FAttachments.IterateObjects(AItem);
  end;

  FreeMem(AMsg, ASize);
end;

function TTVSPParamGroupMapEntry.Add(AValue: TTVSPParamGroupedValue): TEnumerator;
begin
  FLock.Beginwrite;
  Result:=FList.AddAt(AValue);
  FLock.Endwrite;

  SendMsgValGroup(tcVALGROUP, AValue.Owner.Key);
end;

procedure TTVSPParamGroupMapEntry.Remove(AValue: TTVSPParamGroupedValue);
begin
  Assert(AValue.FParamGroupMapPos <> nil);

  FLock.Beginwrite;
  AValue.FParamGroupMapPos.Remove;
  FLock.Endwrite;

  SendMsgValGroup(tcDELETEVALGROUP, AValue.Owner.Key);
  with AValue do begin
    FParamGroupMapPos.Destroy;
    FParamGroupMapPos:=nil;
  end;
end;

function TTVSPParamGroupMapEntry.Subscribe(AAttachment: TTVSPServerAttachment): Boolean;
var
  AItem: TObject;
  AMsg : PSMValGroup;
  ASize: Cardinal;
begin
  Result:=FAttachments.AddObject(AAttachment);

  ASize:=MakeMsgValGroup(tcVALGROUP, NULLVISID, &Type, Key, AMsg);

  FLock.Beginread;

  for AItem in FList do begin
    AMsg^.GroupID:=TTVSPParamGroupedValue(AItem).Owner.Key;
    AAttachment.Send(AMsg, ASize);
  end;

  FLock.Endread;

  FreeMem(AMsg, ASize);
end;

{%ENDREGION}
{%REGION TTVSPParamGroupMap}

constructor TTVSPParamGroupMap.Create;
begin
  inherited Create(NULLVISID);
end;

function TTVSPParamGroupMap.CreateValue(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType): TTVSPBasicValue;
begin
  Result:=TTVSPParamGroupMapEntry.Create(AOwner, AName, AType);
end;

function TTVSPParamGroupMap.GetItem(AName: string; AType: TPParamType): TTVSPParamGroupMapEntry;
begin
  Result:=TTVSPParamGroupMapEntry(inherited Items[AName, AType]);
end;

{%ENDREGION}

var
  LParamGroupMap: TTVSPParamGroupMap = nil;

function ParamGroupMap: TTVSPParamGroupMap;
begin
  Assert(LParamGroupMap <> nil);
  Result:=LParamGroupMap;
end;

procedure Init;
begin
  LParamGroupMap:=TTVSPParamGroupMap.Create;
end;

procedure Done;
begin
  LParamGroupMap.Destroy;
  LParamGroupMap:=nil;
end;

end.

