unit ArrayListedObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MInterfacedObject, PluginType, ObjectClassBasic;

type
  TObjectArray         = class;
  TArrayListedObject   = TObjectItem;

  TArrayListedContainer= class (TObjectContainer)
  private
    FIndex: Integer;
    FOwner: TObjectArray;
  public
    constructor Create(AOwner: TObjectArray; AIndex: Integer; AObject: TObjectItem = nil; AOwnsObject: Boolean = false);
    destructor Destroy; override;
  end;

  TObjectRemoveEvent   = procedure (AObject: TArrayListedObject; Index: Integer) of object;

  TObjectArray         = class
  private
    FItems      : array of TArrayListedContainer;
    FOnClosed   : TObjectRemoveEvent;
    FOwnsObjects: Boolean;
    function GetItem(AIndex: Integer): TArrayListedObject;
    procedure SetItem(AIndex: Integer; AItem: TArrayListedObject);
    procedure RemoveItem(const AIndex: Integer);
    function GetCount: Integer;
  public
    constructor Create(AOwnsObjects: Boolean = true);
    destructor Destroy; override;
    procedure AddItem(AItem: TArrayListedObject);
    procedure DeleteItem(AIndex: Integer);
    procedure InsertItem(AItem: TArrayListedObject; AIndex: Integer);
    procedure Clear;
    procedure AddEmptyItems(ACount: Integer);

    property Items[AIndex: Integer]: TArrayListedObject read GetItem write SetItem; default;
  published
    property Count: Integer read GetCount;
    property OnClosed: TObjectRemoveEvent read FOnClosed write FOnClosed;
  end;

implementation

{TObjectArray}

constructor TObjectArray.Create(AOwnsObjects: Boolean = true);
begin
  inherited Create;
  FOwnsObjects:=AOwnsObjects;
end;

destructor TObjectArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TObjectArray.Clear;
var
  I    : Integer;
  AItem: TArrayListedContainer;
begin
  for I:=Length(FItems)-1 downto 0 do begin
    AItem:=FItems[I];
    if AItem<>nil then begin
      AItem.FOwner:=nil;
      AItem.Destroy;
    end;
  end;
  SetLength(FItems,0);
end;

procedure TObjectArray.AddEmptyItems(ACount: Integer);
var
  I,L: Integer;
begin
  L:=Length(FItems);
  SetLength(FItems,L+ACount);
  for I:=L to L+ACount-1 do FItems[I]:=TArrayListedContainer.Create(Self,I,nil,FOwnsObjects);
end;

procedure TObjectArray.RemoveItem(const AIndex: Integer);
var
  I,L         : Integer;
  AItem,ARItem: TArrayListedContainer;
begin
  ARItem:=FItems[AIndex];
  L:=Length(FItems);
  for I:=AIndex to L-2 do begin
    AItem:=FItems[I+1];
    FItems[I]:=AItem;
    AItem.FIndex:=I;
  end;
  SetLength(FItems,L-1);
  if Assigned(FOnClosed) then FOnClosed(ARItem.Content,AIndex);
end;

function TObjectArray.GetCount: Integer;
begin
  Result:=Length(FItems);
end;

procedure TObjectArray.AddItem(AItem: TArrayListedObject);
var
  L: Integer;
begin
  L:=Length(FItems);
  SetLength(FItems,L+1);
  FItems[L]:=TArrayListedContainer.Create(Self,L,AItem,FOwnsObjects);
end;

procedure TObjectArray.DeleteItem(AIndex: Integer);
begin
  FItems[AIndex].Destroy;
end;

procedure TObjectArray.InsertItem(AItem: TArrayListedObject; AIndex: Integer);
var
  I,L   : Integer;
  ARItem: TArrayListedContainer;
begin
  L:=Length(FItems);
  SetLength(FItems,L+1);
  for I:=L downto AIndex+1 do begin
    ARItem:=FItems[I-1];
    FItems[I]:=ARItem;
    ARItem.FIndex:=I;
  end;
  FItems[AIndex]:=TArrayListedContainer.Create(Self,AIndex,AItem,FOwnsObjects);
end;

function TObjectArray.GetItem(AIndex: Integer): TArrayListedObject;
begin
  Result:=FItems[AIndex].Content;
end;

procedure TObjectArray.SetItem(AIndex: Integer; AItem: TArrayListedObject);
begin
  FItems[AIndex].Content:=AItem;
end;

{TArrayListedContainer}

constructor TArrayListedContainer.Create(AOwner: TObjectArray; AIndex: Integer; AObject: TObjectItem = nil; AOwnsObject: Boolean = false);
begin
  inherited Create(AObject,AOwnsObject);
  FIndex:=AIndex;
  FOwner:=AOwner;
end;

destructor TArrayListedContainer.Destroy;
begin
  if FOwner<>nil then FOwner.RemoveItem(FIndex);
  inherited Destroy;
end;

end.

