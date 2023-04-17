unit TVSPGroups;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csl, HashMap, SyncObjs, MapKeys, VisType2, Enumerators,
  TVSPType, ObjectClasses, ObjectClassBasic, GUIDop, OrderedObjectList,
  LockedObjects;

type
  TConditionCheckEvent = function: Boolean of object;
  TTVSPBasicGroup      = class;

  TTVSPValueKey        = class (TStringKey)
  strict private
    FType : TPParamType;
    function GetMemSize: Cardinal;
    function GetSourceCount: Cardinal;
    function GetValueSize: Cardinal;
  public
    constructor Create(AName: string; AType: TPParamType);
    function Equals(AObject: TObject): Boolean; override;
    function GetHashCode: PtrInt; override;
    property MemSize: Cardinal read GetMemSize;
    property SourceCount: Cardinal read GetSourceCount;
    property &Type: TPParamType read FType;
    property ValueSize: Cardinal read GetValueSize;
  end;

  TTVSPBasicValue      = class (TTVSPValueKey)
  strict private
    FOwner: TTVSPBasicGroup;
  strict protected
    procedure Delete; virtual;
  public
    constructor Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
    property Owner: TTVSPBasicGroup read FOwner;
  end;

  TTVSPIndexedValue    = class (TTVSPBasicValue)
  strict private
    FIndex: TTVSPIndex;
  public
    constructor Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
    destructor Destroy; override;
    property Index: TTVSPIndex read FIndex;
  end;

  TTVSPBasicGroup      = class (TGUIDKey)
  strict private
    FMap       : TMap;
    FLock      : TCriticalSection;
    FObjects   : TLockedObjects;
    function GetItem(AName: string; AType: TPParamType): TTVSPBasicValue;
  private
    function Delete(AValue: TTVSPBasicValue): Boolean;
  strict protected
    function CreateValue(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType): TTVSPBasicValue; virtual;
    property Objects: TLockedObjects read FObjects;
  public
    constructor Create(AID: TGUID);
    destructor Destroy; override;
    function IterateValues(var AIterator: TObject): TTVSPBasicValue;
    property Items[AName: string; AType: TPParamType]: TTVSPBasicValue read GetItem; default;
  end;

  TTVSPBasicGroups     = class
  strict private
    FMap    : TMap;
    FLock   : TCriticalSection;
    function GetItem(AID: TGUID): TTVSPBasicGroup;
    function GetItemOrNil(AID: TGUID): TTVSPBasicGroup;
  strict protected
    function CreateGroup(AID: TGUID): TTVSPBasicGroup; virtual;
  protected
    //Takes a Condition as function and evaluates it during the lock
    //returns true if the group was successfully deleted or the Condition was false
    function DeleteGroup(AKey: TGUIDKey; ACondition: TConditionCheckEvent): Boolean;
  public
    constructor Create(ACreateNull: Boolean = true);
    destructor Destroy; override;
    function StartIterate: TObject;
    function Iterate(APosition: TObject): TTVSPBasicGroup;
    function Count(APosition: TObject): Cardinal;
    property ItemsOrNil[AID: TGUID]: TTVSPBasicGroup read GetItemOrNil;
    property Items[AID: TGUID]: TTVSPBasicGroup read GetItem; default;
  end;

  TTVSPIndexer         = class
  strict private
    FItems : array of TTVSPIndexedValue;
    FLength: Cardinal;
    FLock  : TMultiReadExclusiveWriteSynchronizer;
    procedure Expand; inline;
    function GetItem(AIndex: TTVSPIndex): TTVSPIndexedValue;
  private
    function AddValue(AValue: TTVSPIndexedValue): TTVSPIndex;
    function RemoveValue(AIndex: TTVSPIndex): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    property Items[AIndex: TTVSPIndex]: TTVSPIndexedValue read GetItem; default;
  end;

procedure Init; inline;
procedure Done; inline;
function GetIndexer: TTVSPIndexer; inline;
property Indexer: TTVSPIndexer read GetIndexer;

implementation

{%REGION TTVSPValueKey}

constructor TTVSPValueKey.Create(AName: string; AType: TPParamType);
begin
  inherited Create(AName);
  FType:=AType;
end;

function TTVSPValueKey.Equals(AObject: TObject): Boolean;
begin
  Result:=AObject.InheritsFrom(TTVSPValueKey)
      and inherited Equals(AObject)
      and (TTVSPValueKey(AObject).&Type = FType);
end;

function TTVSPValueKey.GetHashCode: PtrInt;
begin
  Result:=inherited GetHashCode xor PtrInt(FType);
end;

function TTVSPValueKey.GetMemSize: Cardinal;
begin
  Result:=TVSPParamMemSize(FType);
end;

function TTVSPValueKey.GetSourceCount: Cardinal;
begin
  Result:=TVSPParamSourceCount(FType);
end;

function TTVSPValueKey.GetValueSize: Cardinal;
begin
  Result:=TVSPParamValueSize(FType);
end;

{%ENDREGION}
{%REGION TTVSPBasicValue}

constructor TTVSPBasicValue.Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
begin
  inherited Create(AName, AType);
  FOwner:=AOwner;
end;

procedure TTVSPBasicValue.Delete;
var
  AResult: Boolean;
begin
  AResult:=FOwner.Delete(Self);
  Assert(AResult);
end;

{%ENDREGION}
{%REGION TTVSPIndexedValue}

constructor TTVSPIndexedValue.Create(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType);
begin
  inherited Create(AOwner, AName, AType);
  FIndex:=Indexer.AddValue(Self);
end;

destructor TTVSPIndexedValue.Destroy;
begin
  Indexer.RemoveValue(FIndex);
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TTVSPBasicGroup}

constructor TTVSPBasicGroup.Create(AID: TGUID);
begin
  inherited Create(AID);
  FObjects:=TLockedObjects.Create;
  FLock:=TCriticalSection.Create;
  FMap:=THashMap.Create;
end;

destructor TTVSPBasicGroup.Destroy;
begin
  FMap.Clean;
  FMap.Destroy;
  FLock.Destroy;
  FObjects.Destroy;
  inherited Destroy;
end;

function TTVSPBasicGroup.CreateValue(AOwner: TTVSPBasicGroup; AName: string; AType: TPParamType): TTVSPBasicValue;
begin
  Result:=TTVSPBasicValue.Create(AOwner, AName, AType);
end;

function TTVSPBasicGroup.Delete(AValue: TTVSPBasicValue): Boolean;
begin
  FLock.Enter;

  Result:=FMap.Remove(AValue) = AValue;
  AValue.Destroy;

  FLock.Leave;
end;

function TTVSPBasicGroup.GetItem(AName: string; AType: TPParamType): TTVSPBasicValue;
var
  AKey: TTVSPValueKey;
  AObj: TObject;
begin
  AKey:=TTVSPValueKey.Create(AName, AType);

  FLock.Enter;

  AObj:=FMap[AKey];
  if AObj<>nil then begin
    Result:=TTVSPBasicValue(AObj)
  end else begin
    Result:=CreateValue(Self, AName, AType);
    FMap.Add(Result, Result);
  end;

  FLock.Leave;
  AKey.Destroy;
end;

function TTVSPBasicGroup.IterateValues(var AIterator: TObject): TTVSPBasicValue;
var
  AEnumerator: TEnumerator;
begin
  if AIterator = nil then begin
    FLock.Enter;
    AIterator:=FMap.Enumerator;
  end;
  AEnumerator:=TEnumerator(AIterator);
  if AEnumerator.MoveNext then begin
    Result:=TTVSPBasicValue(AEnumerator.Current);
  end else begin
    AEnumerator.Destroy;
    FLock.Leave;
    AIterator:=nil;
    Result:=nil;
  end;
end;

{
procedure TTVSPBasicGroup.IterateObjects(var AIterator: TObjectListItem);
begin
  if AIterator = nil then begin
    FObjectLock.Beginread;
    AIterator:=FObjects.First;
  end else AIterator:=AIterator.Next;
  if AIterator = nil
    then FObjectLock.Endread;
end;

function TTVSPBasicGroup.AddObject(AObject: TObjectItem): Boolean;
var
  AItem: TObjectListItem;
begin
  FObjectLock.Beginwrite;

  //check if already there
  AItem:=FObjects.First;
  while AItem<>nil do begin
    if AItem.Content = AObject then begin
      FObjectLock.EndWrite;
      Result:=false;
      exit;
    end;
    AItem:=AItem.Next;
  end;
  //insert
  FObjects.Add(AObject);

  FObjectLock.EndWrite;
  Result:=true;
end;
}

{%ENDREGION}
{%REGION TTVSPBasicGroups}

constructor TTVSPBasicGroups.Create(ACreateNull: Boolean = true);
var
  AGroup: TTVSPBasicGroup;
begin
  inherited Create;
  FLock:=TCriticalSection.Create;
  FMap:=THashMap.Create;
  if ACreateNull then begin
    AGroup:=CreateGroup(NULLPRESETID);
    FMap.Add(AGroup, AGroup);
  end;
end;

destructor TTVSPBasicGroups.Destroy;
begin
  FMap.Clean;
  FMap.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

function TTVSPBasicGroups.CreateGroup(AID: TGUID): TTVSPBasicGroup;
begin
  Result:=TTVSPBasicGroup.Create(AID);
end;

function TTVSPBasicGroups.DeleteGroup(AKey: TGUIDKey; ACondition: TConditionCheckEvent): Boolean;
begin
  Assert(AKey <> nil);
  FLock.Enter;

  if (not Assigned(ACondition)) or ACondition()
    then Result:=FMap.Delete(AKey)
    else Result:=true;

  FLock.Leave;
end;

function TTVSPBasicGroups.GetItem(AID: TGUID): TTVSPBasicGroup;
var
  AKey : TGUIDKey;
begin
  AKey:=TGUIDKey.Create(AID);

  FLock.Enter;

  Result:=FMap.Items[AKey] as TTVSPBasicGroup;
  if Result=nil then begin
    Result:=CreateGroup(AID);
    FMap.Add(Result, Result);
  end;

  FLock.Leave;
  AKey.Destroy;
end;

function TTVSPBasicGroups.GetItemOrNil(AID: TGUID): TTVSPBasicGroup;
var
  AKey : TGUIDKey;
begin
  AKey:=TGUIDKey.Create(AID);

  FLock.Enter;
  Result:=FMap.Items[AKey] as TTVSPBasicGroup;
  FLock.Leave;

  AKey.Destroy;
end;

function TTVSPBasicGroups.StartIterate: TObject;
begin
  FLock.Enter;
  Result:=FMap.Enumerator;
end;

function TTVSPBasicGroups.Iterate(APosition: TObject): TTVSPBasicGroup;
var
  AEnumerator: TEnumerator;
begin
  Assert((APosition<>nil) and (APosition is TEnumerator));
  AEnumerator:=TEnumerator(APosition);
  if AEnumerator.MoveNext then begin
    Result:=TTVSPBasicGroup(AEnumerator.Current);
  end else begin
    APosition.Destroy;
    Result:=nil;
    FLock.Leave;
  end;
end;

function TTVSPBasicGroups.Count(APosition: TObject): Cardinal;
begin
  //this might be called only in iterations...
  Assert((APosition<>nil) and (APosition is TEnumerator));
  Result:=FMap.Count;
end;

{%ENDREGION}
{%REGION TTVSPIndexer}

constructor TTVSPIndexer.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FLength:=0;
  SetLength(FItems, 1);
end;

destructor TTVSPIndexer.Destroy;
begin
  FLock.Destroy;
  SetLength(FItems, 0);
  inherited Destroy;
end;

procedure TTVSPIndexer.Expand; inline;
begin
  Inc(FLength);
  if FLength > Length(FItems)
    then SetLength(FItems, Length(FItems) * 2);
end;

function TTVSPIndexer.GetItem(AIndex: TTVSPIndex): TTVSPIndexedValue;
begin
  FLock.Beginread;

  if (AIndex >= 0) and (AIndex < FLength)
    then Result:=FItems[AIndex]
    else Result:=nil;

  FLock.Endread;
end;

function TTVSPIndexer.AddValue(AValue: TTVSPIndexedValue): TTVSPIndex;
begin
  FLock.Beginwrite;

  Expand;
  Result:=FLength-1;
  FItems[Result]:=AValue;

  FLock.Endwrite;
end;

function TTVSPIndexer.RemoveValue(AIndex: TTVSPIndex): Boolean;
begin
  FLock.Beginwrite;

  Result:=FItems[AIndex] <> nil;
  FItems[AIndex]:=nil;

  FLock.Endwrite;
end;

{%ENDREGION}

var
  LIndexer: TTVSPIndexer;

function GetIndexer: TTVSPIndexer; inline;
begin
  Result:=LIndexer;
end;

procedure Init; inline;
begin
  LIndexer:=TTVSPIndexer.Create;
end;

procedure Done; inline;
begin
  LIndexer.Destroy;
end;

end.

