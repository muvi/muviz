unit UIDArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvClasses;

type
  TUID               = Integer;
  TUIDIterator       = TUID;
  TObjDataCompareFunc= function (AObject: TObject; const AData): Boolean of object;
  TUIDArray          = class
  private
    FItems    : array of TObject;
    FFreeItems: TLList;
    FIsEqual  : TObjDataCompareFunc;
    function ItemCanBefore(const ANew,AExisting): Boolean; stdcall;
    function GetCount: TUID;
  protected
    function GetItem(const AIndex: TUID): TObject; virtual;
    function FreeItemID: TUID;
    function _RemoveItem(const AID: TUID): TObject;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure _SetCount(const ACount: TUID);
    procedure _SetItem(const AItem: TObject; const AIndex: TUID); virtual;
    procedure _MarkDeleted(const AIndex: TUID);
    function IterFirst: TUIDIterator;
    function IterNext(var Iterator: TUIDIterator): TObject;
    function AddItem(const AItem: TObject): TUID; virtual;
    procedure DeleteItem(const AID: TUID);
    function RemoveItem(const AID: TUID): TObject; virtual;
    procedure Clear;
    procedure SoftClear;
    function Empty: Boolean;
    function FindIndex(const AData): TUID;
    function Find(const AData): TObject;
    function SetToID(const AID: TUID; AItem: TObject): TObject; virtual;
    function ValidID(const AID: TUID): Boolean;
    property Count: TUID read GetCount;
    property IsEqual: TObjDataCompareFunc read FIsEqual write FIsEqual;
    property Items[const AIndex: TUID]: TObject read GetItem; default;
  end;

implementation

{TUIDArray}

constructor TUIDArray.Create;
begin
  inherited Create;
  FFreeItems:=TLList.Create(SizeOf(TUID));
  FFreeItems.OnCanBefore:=@ItemCanBefore;
  FIsEqual:=nil;
end;

destructor TUIDArray.Destroy;
begin
  Clear;
  FFreeItems.Destroy;
  inherited Destroy;
end;

procedure TUIDArray._SetCount(const ACount: TUID);
begin
  SetLength(FItems,ACount);
end;

procedure TUIDArray._SetItem(const AItem: TObject; const AIndex: TUID);
begin
  FItems[AIndex]:=AItem;
end;

procedure TUIDArray._MarkDeleted(const AIndex: TUID);
begin
  FItems[AIndex]:=nil;
  FFreeItems.InsertSorted(AIndex);
end;

function TUIDArray.IterFirst: TUIDIterator;
begin
  Result:=-1;
end;

function TUIDArray.IterNext(var Iterator: TUIDIterator): TObject;
var
  L: TUID;
begin
  Inc(Iterator);
  L:=Length(FItems);
  if Iterator>=L then begin
    Result:=nil;
    exit;
  end;
  Result:=Items[Iterator];
  while Result=nil do begin
    Inc(Iterator);
    if Iterator>=L then begin
      Result:=nil;
      exit;
    end;
    Result:=Items[Iterator];
  end;
end;

function TUIDArray.ItemCanBefore(const ANew,AExisting): Boolean; stdcall;
var
  ANew2     : TUID absolute ANew;
  AExisting2: TUID absolute AExisting;
begin
  Result:=ANew2<AExisting2;
end;

function TUIDArray.GetCount: TUID;
begin
  Result:=Length(FItems);
end;

function TUIDArray.GetItem(const AIndex: TUID): TObject;
begin
  Result:=FItems[AIndex];
end;

function TUIDArray.FreeItemID: TUID;
begin
  if FFreeItems.Empty then begin
    Result:=Length(FItems);
    SetLength(FItems,Result+1);
  end else begin
    FFreeItems.ToFirst;
    FFreeItems.Get(Result);
    FFreeItems.Delete;
  end;
end;

function TUIDArray.AddItem(const AItem: TObject): TUID;
begin
  Result:=FreeItemID;
  FItems[Result]:=AItem;
end;

function TUIDArray._RemoveItem(const AID: TUID): TObject;
var
  AItem           : ^TObject;
  L,ADelID,ALDelID: TUID;
begin
  AItem:=@FItems[AID];
  Result:=AItem^;
  L:=Length(FItems);
  if AID<L-1 then begin
    FFreeItems.InsertSorted(AID);
    AItem^:=nil;
  end else begin
    if FFreeItems.Empty then SetLength(FItems,L-1) else begin
      FFreeItems.ToLast;
      FFreeItems.Get(ADelID);
      ALDelID:=L-1;
      while ADelID=ALDelID-1 do begin
        FFreeItems.DeleteI;
        Dec(ALDelID);
        if FFreeItems.Empty then break;
        FFreeItems.Get(ADelID);
      end;
      SetLength(FItems,ALDelID);
    end;
  end;
end;

procedure TUIDArray.DeleteItem(const AID: TUID);
begin
  _RemoveItem(AID).Destroy;
end;

function TUIDArray.RemoveItem(const AID: TUID): TObject;
begin
  Result:=_RemoveItem(AID);
end;

procedure TUIDArray.Clear;
var
  I    : Integer;
  AItem: TObject;
begin
  for I:=0 to Length(FItems)-1 do begin
    AItem:=FItems[I];
    if AItem<>nil then AItem.Destroy;
  end;
  SetLength(FItems,0);
  FFreeItems.Clear;
end;

procedure TUIDArray.SoftClear;
begin
  SetLength(FItems,0);
  FFreeItems.Clear;
end;

function TUIDArray.Empty: Boolean;
begin
  Result:=(Length(FItems)=0);
end;

function TUIDArray.FindIndex(const AData): TUID;
var
  I: TUID;
begin
  for I:=0 to Length(FItems)-1 do if FIsEqual(Items[I],AData) then begin
    Result:=I;
    exit;
  end;
  Result:=-1;
end;

function TUIDArray.Find(const AData): TObject;
var
  I: TUID;
begin
  for I:=0 to Length(FItems)-1 do begin
    Result:=Items[I];
    if FIsEqual(Result,AData) then exit;
  end;
  Result:=nil;
end;

function TUIDArray.SetToID(const AID: TUID; AItem: TObject): TObject;
begin
  if AID>=Length(FItems) then begin
    SetLength(FItems,AID+1);
    Result:=nil;
  end else Result:=Items[AID];
  _SetItem(AItem,AID);
  //FItems[AID]:=AItem;
end;

function TUIDArray.ValidID(const AID: TUID): Boolean;
begin
  Result:=((AID>=0) and (AID<Length(FItems)));
  if Result then Result:=FItems[AID]<>nil;
end;

end.

