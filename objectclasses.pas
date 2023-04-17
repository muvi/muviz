unit ObjectClasses;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClassBasic, AdvancedObjectClassBasic;

type
  TObjectList       = class;

  TObjectListItem   = class (TAdvancedObjectContainer)
  private
    FNext              : TObjectListItem;
    FPrev              : TObjectListItem;
    FDoRearangeOnDelete: Boolean;
  public
    constructor Create(AOwner: TObjectList; APrev,ANext: TObjectListItem; AObject: TObjectItem = nil; AOwnsObjects: Boolean = false);
    destructor Destroy; override;
    procedure AddNext(AObject: TObjectItem = nil);
    procedure AddPrev(AObject: TObjectItem = nil);
    property Next: TObjectListItem read FNext;
    property Prev: TObjectListItem read FPrev;
  end;

  TObjectList       = class (TAutomaticNotifyingObjectStructure)
  private
    FFirst           : TObjectListItem;
    FLast            : TObjectListItem;
    FOwnsObjects     : Boolean;
    FCount           : Cardinal;
    FOnDeleteObject  : TNotifyEvent;
    FOnBeforeDeletion: TNotifyEvent;
    function GetEmpty: Boolean;
    procedure ObjectDeleted;
    procedure BeforeDeletion;
  public
    constructor Create(AOwnsObjects: Boolean = false);
    destructor Destroy; override;
    procedure Clear;
    procedure AddFirst(AObject: TObjectItem = nil);
    procedure Add(AObject: TObjectItem = nil); virtual;
    property Count: Cardinal read FCount;
    property Empty: Boolean read GetEmpty;
    property First: TObjectListItem read FFirst;
    property Last: TObjectListItem read FLast;
    property OnBeforeDeletion: TNotifyEvent read FOnBeforeDeletion write FOnBeforeDeletion;
    property OnDeleteObject: TNotifyEvent read FOnDeleteObject write FOnDeleteObject;
  end;

  TObjectArrayItem  = class;

  TBasicObjectArray = class
  strict private
    FOwnsObjects: Boolean;
  strict protected
    function GetItem(Index: Cardinal): TObjectItem; virtual; abstract;
    procedure SetItem(Index: Cardinal; Value: TObjectItem); virtual; abstract;
    function GetCount: Cardinal; virtual; abstract;
    procedure SetCount(Value: Cardinal); virtual; abstract;
    property OwnsObjects: Boolean read FOwnsObjects;
  protected
    procedure ItemDeleted(AIndex: Cardinal); virtual; abstract;
  public
    constructor Create(AOwnsObjects: Boolean = false);
    procedure Clear; virtual; abstract;
    property Items[Index: Cardinal]: TObjectItem read GetItem write SetItem; default;
    property Count: Cardinal read GetCount write SetCount;
  end;

  TObjectArrayItem  = class (TObjectContainer)
  private
    FIndex: Cardinal;
    FOwner: TBasicObjectArray;
  public
    constructor Create(AOwner: TBasicObjectArray; AIndex: Cardinal; AObject: TObjectItem = nil; AOwnsObjects: Boolean = false);
    destructor Destroy; override;
    property Index: Cardinal read FIndex write FIndex;
  end;

  TNilObjectArray   = class (TBasicObjectArray)
  private
    FItems      : array of TObjectArrayItem;
  strict protected
    function GetItem(Index: Cardinal): TObjectItem; override;
    procedure SetItem(Index: Cardinal; Value: TObjectItem); override;
    function GetCount: Cardinal; override;
    procedure SetCount(Value: Cardinal); override;
  protected
    procedure ItemDeleted(AIndex: Cardinal); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
  end;

implementation

{%REGION TObjectListItem}

constructor TObjectListItem.Create(AOwner: TObjectList; APrev,ANext: TObjectListItem; AObject: TObjectItem = nil; AOwnsObjects: Boolean = false);
begin
  inherited Create(AOwner, AObject, AOwnsObjects);
  FDoRearangeOnDelete:=true;
  FNext:=ANext;
  FPrev:=APrev;
  Inc(AOwner.FCount);
end;

destructor TObjectListItem.Destroy;
begin
  if FDoRearangeOnDelete then begin
    TObjectList(Owner).BeforeDeletion;
    Dec(TObjectList(Owner).FCount);
    if FPrev<>nil
      then FPrev.FNext:=FNext
      else TObjectList(Owner).FFirst:=FNext;
    if FNext<>nil
      then FNext.FPrev:=FPrev
      else TObjectList(Owner).FLast:=FPrev;
    TObjectList(Owner).ObjectDeleted;
  end;
  inherited Destroy;
end;

procedure TObjectListItem.AddNext(AObject: TObjectItem = nil);
var
  AItem: TObjectListItem;
begin
  AItem:=TObjectListItem.Create(TObjectList(Owner),Self,FNext,AObject,OwnsObject);
  if FNext<>nil
    then FNext.FPrev:=AItem
    else TObjectList(Owner).FLast:=AItem;
  FNext:=AItem;
end;

procedure TObjectListItem.AddPrev(AObject: TObjectItem = nil);
var
  AItem: TObjectListItem;
begin
  AItem:=TObjectListItem.Create(TObjectList(Owner),FPrev,Self,AObject,OwnsObject);
  if FPrev<>nil
    then FPrev.FNext:=AItem
    else TObjectList(Owner).FFirst:=AItem;
  FPrev:=AItem;
end;

{%ENDREGION}
{%REGION TObjectList}

constructor TObjectList.Create(AOwnsObjects: Boolean = false);
begin
  inherited Create;
  FFirst:=nil;
  FLast:=nil;
  FOwnsObjects:=AOwnsObjects;
  FCount:=0;
end;

destructor TObjectList.Destroy;
begin
  FOnDeleteObject:=nil;
  Clear;
  inherited Destroy;
end;

procedure TObjectList.Clear;
var
  AItem,AItem2: TObjectListItem;
begin
  AItem:=FFirst;
  while AItem<>nil do begin
    AItem2:=AItem;
    AItem:=AItem.Next;
    AItem2.FDoRearangeOnDelete:=false;
    AItem2.Destroy;
  end;
  FFirst:=nil;
  FLast:=nil;
  FCount:=0;
end;

procedure TObjectList.AddFirst(AObject: TObjectItem = nil);
var
  AItem: TObjectListItem;
begin
  AItem:=TObjectListItem.Create(Self,nil,FFirst,AObject,FOwnsObjects);
  if FFirst<>nil
    then FFirst.FPrev:=AItem
    else FLast:=AItem;
  FFirst:=AItem;
end;

procedure TObjectList.Add(AObject: TObjectItem = nil);
var
  AItem: TObjectListItem;
begin
  AItem:=TObjectListItem.Create(Self,FLast,nil,AObject,FOwnsObjects);
  if FLast<>nil
    then FLast.FNext:=AItem
    else FFirst:=AItem;
  FLast:=AItem;
end;

function TObjectList.GetEmpty: Boolean;
begin
  Result:=(FFirst=nil);
end;

procedure TObjectList.ObjectDeleted;
begin
  if Assigned(FOnDeleteObject)
    then FOnDeleteObject(Self);
end;

procedure TObjectList.BeforeDeletion;
begin
  if Assigned(FOnBeforeDeletion)
    then FOnBeforeDeletion(Self);
end;

{%ENDREGION}
{%REGION TObjectArrayItem}

constructor TObjectArrayItem.Create(AOwner: TBasicObjectArray; AIndex: Cardinal; AObject: TObjectItem = nil; AOwnsObjects: Boolean = false);
begin
  inherited Create(AObject,AOwnsObjects);
  FIndex:=AIndex;
  FOwner:=AOwner;
end;

destructor TObjectArrayItem.Destroy;
begin
  FOwner.ItemDeleted(FIndex);
  inherited Destroy;
end;

{%ENDREGION}
{%REGION TBasicObjectArray}

constructor TBasicObjectArray.Create(AOwnsObjects: Boolean = false);
begin
  inherited Create;
  FOwnsObjects:=AOwnsObjects;
end;

{%ENDREGION}
{%REGION TNilObjectArray}

destructor TNilObjectArray.Destroy;
begin
  SetCount(0);
  inherited Destroy;
end;

procedure TNilObjectArray.ItemDeleted(AIndex: Cardinal);
begin
  FItems[AIndex]:=nil;
end;

procedure TNilObjectArray.Clear;
var
  I: Integer;
begin
  for I:=0 to Length(FItems)-1 do if FItems[I]<>nil then FItems[I].Destroy;
end;

function TNilObjectArray.GetItem(Index: Cardinal): TObjectItem;
var
  AItem: TObjectArrayItem;
begin
  AItem:=FItems[Index];
  if AItem<>nil
    then Result:=AItem.Content
    else Result:=nil;
end;

procedure TNilObjectArray.SetItem(Index: Cardinal; Value: TObjectItem);
var
  AItem: TObjectArrayItem;
begin
  AItem:=FItems[Index];
  if AItem<>nil then AItem.Destroy;
  if Value<>nil
    then FItems[Index]:=TObjectArrayItem.Create(Self,Index,Value,OwnsObjects)
    else FItems[Index]:=nil;
end;

function TNilObjectArray.GetCount: Cardinal;
begin
  Result:=Length(FItems);
end;

procedure TNilObjectArray.SetCount(Value: Cardinal);
var
  I,L: Integer;
begin
  L:=Length(FItems);
  for I:=Value to L-1 do if FItems[I]<>nil then FItems[I].Destroy;
  SetLength(FItems,Value);
  for I:=L to Value-1 do FItems[I]:=nil;
end;

end.

