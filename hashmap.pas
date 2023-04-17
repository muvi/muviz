unit HashMap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csl, Enumerators, LinkedList;

type
  THashMapItem        = class
  private
    FKey  : TObject;
    FValue: TObject;
  public
    constructor Create(AKey: TObject; AValue: TObject);
    destructor Destroy; override;
    procedure DestroyAndDelete; virtual;
    property Key: TObject read FKey;
    property Value: TObject read FValue;
  end;

  THashMap            = class;

  THashMapEnumerator  = class (TEnumerator)
  private
    FEnumerates    : THashMap;
    FListEnumerator: TEnumerator;
    FIndex         : Integer;
    //for debugging purposes only
    FCount         : Integer;
  strict protected
    function GetCurrent: TObject; override;
  public
    constructor Create(AEnumerates: THashMap);
    destructor Destroy; override;
    function MoveNext: Boolean; override;
    procedure Reset; override;
  end;

  //HashMap Keys have to inherit from THashMapKey
  THashMap            = class (TMap)
  private
    FInitialSize    : Integer;
    FSize           : Integer;
    FBadSize        : Integer;
    FCount          : Integer;
    FCompleteBadSize: Integer;
    FMap            : array of TList;
    procedure Grow(ANewSize: Integer); //inline;
    procedure Shrink(ANewSize: Integer); //inline;
    procedure Insert(AItems: TList); //inline;
    procedure Add(AItem: THashMapItem);
    procedure Empty; //inline;
    function IterateTo(AKey: TObject): TEnumerator;
  protected
    function GetCount: Integer; override;
    function GetItem(AKey: TObject): TObject; override;
    function GetHash(AKey: TObject): Cardinal; inline;
  public
    constructor Create(AInitialSize: Integer = 256; ABadSize: Integer = 10);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator; override;
    procedure Add(AKey: TObject; AValue: TObject); override;
    procedure Clear; override;
    procedure Clean; override;
    function Contains(AKey: TObject): Boolean; override;
    function Remove(AKey: TObject): TObject; override;
    function Delete(AKey: TObject): Boolean; override;
  end;

implementation

const
  GrowHashMapSize = 2;

{%REGION THashMapEnumerator}

constructor THashMapEnumerator.Create(AEnumerates: THashMap);
begin
  inherited Create;
  FEnumerates:=AEnumerates;
  FListEnumerator:=nil;
  FIndex:=-1;
  FCount:=0;
end;

destructor THashMapEnumerator.Destroy;
begin
  if FListEnumerator<>nil
    then FListEnumerator.Destroy;
  inherited Destroy;
end;

function THashMapEnumerator.GetCurrent: TObject;
begin
  if FListEnumerator<>nil
    then Result:=THashMapItem(FListEnumerator.Current).FValue
    else Result:=nil;
end;

function THashMapEnumerator.MoveNext: Boolean;
begin
  while (FListEnumerator=nil) or (not FListEnumerator.MoveNext) do begin
    Inc(FIndex);
    Result:=(FIndex<FEnumerates.FSize);
    if not Result then begin
      Assert(FCount = FEnumerates.Count);
      exit;
    end;
    if FListEnumerator <> nil
      then FListEnumerator.Destroy;
    FListEnumerator:=FEnumerates.FMap[FIndex].Enumerator;
  end;
  //if the loop was never entered...
  Result:=true;
  Inc(FCount);
  Assert(FCount <= FEnumerates.Count);
end;

procedure THashMapEnumerator.Reset;
begin
  if FListEnumerator<>nil
    then FListEnumerator.Destroy;
  FListEnumerator:=nil;
  FIndex:=-1;
  FCount:=0;
end;

{%ENDREGION}
{%REGION THashMapItem}

constructor THashMapItem.Create(AKey: TObject; AValue: TObject);
begin
  inherited Create;
  FKey:=AKey;
  FValue:=AValue;
end;

destructor THashMapItem.Destroy;
begin
  if FKey <> FValue
    then FKey.Destroy;
  inherited Destroy;
end;

procedure THashMapItem.DestroyAndDelete;
begin
  //do not set to nil to make the comparison in Destroy possible
  FValue.Destroy;
  Destroy;
end;

{%ENDREGION}
{%REGION THashMap}

constructor THashMap.Create(AInitialSize: Integer = 256; ABadSize: Integer = 10);
begin
  inherited Create;
  FCount:=0;
  FInitialSize:=AInitialSize;
  FBadSize:=ABadSize;
  Grow(AInitialSize);
end;

destructor THashMap.Destroy;
begin
  FInitialSize:=0;
  Clear;
  inherited Destroy;
end;

function THashMap.GetEnumerator: TEnumerator;
begin
  Result:=THashMapEnumerator.Create(Self);
end;

procedure THashMap.Grow(ANewSize: Integer); //inline;
var
  I,OldSize: Integer;
begin
  OldSize:=Length(FMap);
  SetLength(FMap, ANewSize);
  for I:=OldSize to ANewSize-1
    do FMap[I]:=TLinkedList.Create;
  FSize:=ANewSize;
  FCompleteBadSize:=FBadSize * FSize;
end;

procedure THashMap.Shrink(ANewSize: Integer); //inline;
var
  I: Integer;
begin
  for I:=ANewSize to Length(FMap)-1
    do FMap[I].Destroy;
  SetLength(FMap, ANewSize);
  FSize:=ANewSize;
  FCompleteBadSize:=FBadSize * FSize;
end;

procedure THashMap.Empty; //inline;
var
  I: Integer;
begin
  for I:=0 to Length(FMap)-1
    do FMap[I].Clear;
end;

procedure THashMap.Insert(AItems: TList); //inline;
var
  AItem: TObject;
begin
  for AItem in AItems
    do Add(THashMapItem(AItem));
end;

function THashMap.GetHash(AKey: TObject): Cardinal; inline;
begin
  Result:=PtrUInt(AKey.GetHashCode) mod FSize;
end;

function THashMap.IterateTo(AKey: TObject): TEnumerator;
var
  AHash : Integer;
begin
  AHash:=GetHash(AKey);
  Result:=FMap[AHash].Enumerator;
  while Result.MoveNext
    do if THashMapItem(Result.Current).FKey.Equals(AKey)
      then exit;
  Result.Destroy;
  Result:=nil;
end;

procedure THashMap.Add(AItem: THashMapItem);
var
  AList: TList;
begin
  AList:=FMap[GetHash(AItem.Key)];
  AList.Add(AItem);
  //Reorder HashMap
  if FCount>=FCompleteBadSize then begin
    AList:=TLinkedList.Create(FMap);
    Empty;
    Grow(FSize*GrowHashMapSize);
    Insert(AList);
    AList.Destroy;
  end;
end;

function THashMap.GetItem(AKey: TObject): TObject;
var
  Iter: TEnumerator;
begin
  Iter:=IterateTo(AKey);
  if Iter=nil then begin
    Result:=nil;
    exit;
  end;
  Result:=THashMapItem(Iter.Current).Value;
  Iter.Destroy;
end;

function THashMap.GetCount: Integer;
begin
  Result:=FCount;
end;

procedure THashMap.Add(AKey: TObject; AValue: TObject);
begin
  Add(THashMapItem.Create(AKey, AValue));
  Inc(FCount);
end;

procedure THashMap.Clear;
var
  I: Integer;
begin
  for I:=0 to Length(FMap)-1
    do FMap[I].Clean;
  Shrink(FInitialSize);
  FCount:=0;
end;

procedure THashMap.Clean;
var
  I     : Integer;
  Iter  : TEnumerator;
begin
  for I:=0 to Length(FMap)-1 do begin
    Iter:=FMap[I].GetEnumerator;
    while Iter.MoveNext
      do THashMapItem(Iter.Remove).DestroyAndDelete;
    Iter.Destroy;
  end;
  Shrink(FInitialSize);
  FCount:=0;
end;

function THashMap.Contains(AKey: TObject): Boolean;
begin
  Result:=GetItem(AKey)<>nil;
end;

function THashMap.Remove(AKey: TObject): TObject;
var
  Iter : TEnumerator;
  AItem: THashMapItem;
begin
  Iter:=IterateTo(AKey);
  if Iter=nil then begin
    Result:=nil;
    exit;
  end;
  AItem:=THashMapItem(Iter.Remove);
  Result:=AItem.Value;
  AItem.Destroy;
  Iter.Destroy;
  Dec(FCount);
end;

function THashMap.Delete(AKey: TObject): Boolean;
var
  AItem: TObject;
begin
  AItem:=Remove(AKey);
  Result:=AItem <> nil;
  if Result
    then AItem.Destroy;
end;

{%ENDREGION}

end.

