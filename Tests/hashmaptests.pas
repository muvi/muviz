unit HashMapTests;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, HashMap, csl, MapKeys;

const
  TestObjectCount = 5;

type
  TTestObject = class
  private
    FContent      : Integer;
    FOnDestruction: TNotifyEvent;
  public
    constructor Create(AContent: Integer);
    destructor Destroy; override;
    property Content: Integer read FContent;
    property OnDestruction: TNotifyEvent read FOnDestruction write FOnDestruction;
  end;

  TMapTest     = class (TTestCase)
  private
    FMap            : TMap;
    FItems          : array [0..TestObjectCount-1] of TTestObject;
    procedure TestObjectDestruction(Sender: TObject);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function GetMap: TMap; virtual; abstract;
    function GetTestObjectContent(AIndex: Integer): Integer; virtual;
    function GetTestObjectKey(AIndex: Integer): Integer; virtual;
    function GetTestObjectIndex(AValueContent: Integer): Integer; virtual;
  published
    procedure TestContains;
    procedure TestClear;
    procedure TestClean;
    procedure TestRemove;
    procedure TestDelete;
    procedure TestEnumerator;
    procedure TestEqualHashes;
  end;

  THashMapTest = class (TMapTest)
  protected
    function GetMap: TMap; override;
  end;

implementation

{%REGION TTestObject}

constructor TTestObject.Create(AContent: Integer);
begin
  inherited Create;
  FContent:=AContent;
end;

destructor TTestObject.Destroy;
begin
  if Assigned(FOnDestruction)
    then FOnDestruction(Self);
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TMapTests}

procedure TMapTest.SetUp;
var
  I    : Integer;
  AItem: TTestObject;
begin
  FMap:=GetMap;
  for I:=0 to TestObjectCount-1 do begin
    AItem:=TTestObject.Create(GetTestObjectContent(I));
    AItem.OnDestruction:=@TestObjectDestruction;
    FItems[I]:=AItem;
    FMap.Add(TIntegerKey.Create(GetTestObjectKey(I)), AItem);
  end;
end;

procedure TMapTest.TearDown;
begin
  FMap.Clean;
  FMap.Destroy;
end;

function TMapTest.GetTestObjectContent(AIndex: Integer): Integer;
begin
  Result:=AIndex;
end;

function TMapTest.GetTestObjectKey(AIndex: Integer): Integer;
begin
  Result:=AIndex;
end;

function TMapTest.GetTestObjectIndex(AValueContent: Integer): Integer;
begin
  Result:=AValueContent;
end;

procedure TMapTest.TestObjectDestruction(Sender: TObject);
begin
  FItems[GetTestObjectIndex(TTestObject(Sender).Content)]:=nil;
end;

procedure TMapTest.TestContains;
var
  I   : Integer;
  AKey: TObject;
begin
  for I:=0 to TestObjectCount-1 do begin
    AKey:=TIntegerKey.Create(GetTestObjectKey(I));
    AssertTrue('Map does not contain Key ' + IntToStr(GetTestObjectKey(I)) + ', which was added before.', FMap.Contains(AKey));
    AKey.Destroy;
  end;
end;

procedure TMapTest.TestClear;
var
  I   : Integer;
  AKey: TObject;
begin
  FMap.Clear;
  for I:=0 to TestObjectCount-1 do begin
    AKey:=TIntegerKey.Create(GetTestObjectKey(I));
    AssertFalse('Map contains Key ' + IntToStr(GetTestObjectKey(I)) + ', after it has been cleared.', FMap.Contains(AKey));
    AKey.Destroy;
    AssertNotNull('Item has been destroyed.',FItems[I]);
    AssertEquals('Item value has changed.', GetTestObjectContent(I), FItems[I].Content);
  end;
end;

procedure TMapTest.TestClean;
var
  I   : Integer;
  AKey: TObject;
begin
  FMap.Clean;
  for I:=0 to TestObjectCount-1 do begin
    AKey:=TIntegerKey.Create(GetTestObjectKey(I));
    AssertFalse('Map contains Key ' + IntToStr(GetTestObjectKey(I)) + ', after it has been cleared.', FMap.Contains(AKey));
    AKey.Destroy;
    AssertNull('Item with key ' + IntToStr(GetTestObjectKey(I)) + ' has not been destroyed',FItems[I]);
  end;
end;

procedure TMapTest.TestRemove;
var
  I   : Integer;
  AKey: TObject;
begin
  AKey:=TIntegerKey.Create(GetTestObjectKey(3));
  FMap.Remove(AKey);
  AKey.Destroy;
  for I:=0 to TestObjectCount-1 do begin
    AKey:=TIntegerKey.Create(GetTestObjectKey(I));
    if I=3
      then AssertFalse('Map contains key ' + IntToStr(GetTestObjectKey(I)) + ', after this item has been removed.', FMap.Contains(AKey))
      else AssertTrue('Map does not contain key ' + IntToStr(GetTestObjectKey(I)) + ', after item with key ' + IntToStr(GetTestObjectKey(3)) + ' has been removed.', FMap.Contains(AKey));
    AKey.Destroy;
    AssertNotNull('Item has been destroyed.',FItems[I]);
    AssertEquals('Item value has changed.', GetTestObjectContent(I), FItems[I].Content);
  end;
end;

procedure TMapTest.TestDelete;
var
  I   : Integer;
  AKey: TObject;
begin
  AKey:=TIntegerKey.Create(GetTestObjectKey(3));
  FMap.Delete(AKey);
  AKey.Destroy;
  for I:=0 to TestObjectCount-1 do begin
    AKey:=TIntegerKey.Create(GetTestObjectKey(I));
    if I=3 then begin
      AssertFalse('Map contains key ' + IntToStr(GetTestObjectKey(I)) + ', after this item has been removed.', FMap.Contains(AKey));
      AssertNull('Item with key ' + IntToStr(GetTestObjectKey(I)) + ' has not been destroyed',FItems[I]);
    end else begin
      AssertTrue('Map does not contain key ' + IntToStr(GetTestObjectKey(I)) + ', after item with key ' + IntToStr(GetTestObjectKey(3)) + ' has been removed.', FMap.Contains(AKey));
      AssertNotNull('Item has been destroyed.',FItems[I]);
      AssertEquals('Item value has changed.', GetTestObjectContent(I), FItems[I].Content);
    end;
    AKey.Destroy;
  end;
end;

procedure TMapTest.TestEnumerator;
var
  AItem      : TObject;
  AItemsFound: set of byte;
  AItemIndex : Integer;
begin
  AItemsFound:=[];
  for AItem in FMap do begin
    AItemIndex:=GetTestObjectIndex(TTestObject(AItem).Content);
    if AItemIndex in AItemsFound
      then Fail('Iterated over item ' + IntToStr(AItemIndex) + ' twice.');
    Include(AItemsFound, AItemIndex);
  end;
  if AItemsFound<>[0..TestObjectCount-1]
    then Fail('Missed some items while iterating.');
end;

procedure TMapTest.TestEqualHashes;
var
  I   : Integer;
  AKey: TObject;
begin
  //bad size is intialized with 10
  for I:=0 to 10 do begin
    AKey:=TIntegerKey.Create(42);
    //the value does not matter here
    FMap.Add(AKey, TObject.Create);
  end;
  //ends in endless recursion if the map does not allow equal hashes
end;

{%ENDREGION}
{%REGION THashMapTest}

function THashMapTest.GetMap: TMap;
begin
  Result:=THashMap.Create;
end;

{%ENDREGION}

initialization

  RegisterTest(THashMapTest);
end.

