unit LinkedList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, csl, Enumerators;

type
  TLinkedList           = class;

  TLinkedListItem       = class
  private
    FNext   : TLinkedListItem;
  strict private
    FContent: TObject;
  public
    constructor Create(AContent: TObject; ANext: TLinkedListItem);
    procedure DestroyAndDelete; virtual;
    property Content: TObject read FContent;
    property Next: TLinkedListItem read FNext;
  end;

  TLinkedListEnumerator = class (TEnumerator)
  strict private
    FPrev   : TLinkedListItem;
    FCurrent: TLinkedListItem;
    FOwner  : TLinkedList;
    function DoRemove: TLinkedListItem;
  public
    constructor Create(AOwner: TLinkedList; APrev: TLinkedListItem = nil; ACurrent: TLinkedListItem = nil);
    function GetCurrent: TObject; override;
    function MoveNext: Boolean; override;
    function Remove: TObject; override;
    procedure Delete; override;
    procedure Reset; override;
  end;

  TLinkedList           = class (TList)
  private
    FFirst: TLinkedListItem;
    FLast : TLinkedListItem;
    FCount: Integer;
  protected
    function GetCount: Integer; override;
  public
    constructor Create;
    constructor Create(ACreateFrom: array of TList);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator; override;
    procedure Clean; override;
    procedure Clear; override;
    procedure Add(AObject: TObject); override;
    function AddAt(AObject: TObject): TEnumerator; override;
  end;

implementation

{%REGION TLinkedListItem}

constructor TLinkedListItem.Create(AContent: TObject; ANext: TLinkedListItem);
begin
  FContent:=AContent;
  FNext:=ANext;
end;

procedure TLinkedListItem.DestroyAndDelete;
begin
  FContent.Destroy;
  Destroy;
end;

{%ENDREGION}
{%REGION TLinkedListEnumerator}

constructor TLinkedListEnumerator.Create(AOwner: TLinkedList; APrev: TLinkedListItem = nil; ACurrent: TLinkedListItem = nil);
begin
  FOwner:=AOwner;
  FCurrent:=ACurrent;
  FPrev:=APrev;
end;

function TLinkedListEnumerator.GetCurrent: TObject;
begin
  Result:=FCurrent.Content;
end;

function TLinkedListEnumerator.MoveNext: Boolean;
begin
  FPrev:=FCurrent;
  if FCurrent=nil
    then FCurrent:=FOwner.FFirst
    else FCurrent:=FCurrent.Next;
  Result:=FCurrent<>nil;
end;

function TLinkedListEnumerator.DoRemove: TLinkedListItem;
begin
  Dec(FOwner.FCount);
  if FOwner.FLast=FCurrent
    then FOwner.FLast:=FPrev;
  if FPrev=nil
    then FOwner.FFirst:=FCurrent.Next
    else FPrev.FNext:=FCurrent.Next;
  Result:=FCurrent;
  FCurrent:=FPrev;
end;

function TLinkedListEnumerator.Remove: TObject;
var
  ARemoved: TLinkedListItem;
begin
  ARemoved:=DoRemove;
  Result:=ARemoved.Content;
  ARemoved.Destroy;
end;

procedure TLinkedListEnumerator.Delete;
begin
  DoRemove.DestroyAndDelete;
end;

procedure TLinkedListEnumerator.Reset;
begin
  FCurrent:=FOwner.FFirst;
  FPrev:=nil;
end;

{%ENDREGION}
{%REGION TLinkedList}

constructor TLinkedList.Create;
begin
  inherited Create;
  FFirst:=nil;
  FLast:=nil;
  FCount:=0;
end;

constructor TLinkedList.Create(ACreateFrom: array of TList);
var
  I    : Integer;
  AItem: TObject;
begin
  inherited Create;
  FFirst:=nil;
  FLast:=nil;
  FCount:=0;
  //insert Lists
  for I:=0 to Length(ACreateFrom)-1 do begin
    for AItem in ACreateFrom[I] do begin
      Add(AItem);
    end;
  end;
end;

destructor TLinkedList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLinkedList.Clear;
var
  AItem, ALastItem: TLinkedListItem;
begin
  AItem:=FFirst;
  while AItem<>nil do begin
    ALastItem:=AItem;
    AItem:=AItem.Next;
    ALastItem.Destroy;
  end;
  FFirst:=nil;
  FLast:=nil;
  FCount:=0;
end;

procedure TLinkedList.Clean;
var
  AItem, ALastItem: TLinkedListItem;
begin
  AItem:=FFirst;
  while AItem<>nil do begin
    ALastItem:=AItem;
    AItem:=AItem.Next;
    ALastItem.DestroyAndDelete;
  end;
  FFirst:=nil;
  FLast:=nil;
  FCount:=0;
end;

procedure TLinkedList.Add(AObject: TObject);
var
  ANewItem: TLinkedListItem;
begin
  ANewItem:=TLinkedListItem.Create(AObject, nil);
  if FFirst=nil
    then FFirst:=ANewItem
    else FLast.FNext:=ANewItem;
  FLast:=ANewItem;
  Inc(FCount);
end;

function TLinkedList.AddAt(AObject: TObject): TEnumerator;
var
  ANewItem: TLinkedListItem;
begin
  ANewItem:=TLinkedListItem.Create(AObject, nil);
  if FFirst=nil
    then FFirst:=ANewItem
    else FLast.FNext:=ANewItem;
  Result:=TLinkedListEnumerator.Create(Self, FLast, ANewItem);
  FLast:=ANewItem;
  Inc(FCount);
end;

function TLinkedList.GetEnumerator: TEnumerator;
begin
  Result:=TLinkedListEnumerator.Create(Self);
end;

function TLinkedList.GetCount: Integer;
begin
  Result:=FCount;
end;

{%ENDREGION}

end.

