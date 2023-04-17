unit SrcUIDArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, AdvClasses, SourceType;

type
  TMVSourceIterator  = MVSourceID;
  TSrcDataCompareFunc= function (AObject: IMSource; const AData): Boolean of object;
  TSrcUIDArray       = class
  private
    FItems    : array of IMSource;
    FFreeItems: TLList;
    FIsEqual  : TSrcDataCompareFunc;
    function ItemCanBefore(const ANew,AExisting): Boolean; stdcall;
    function GetCount: MVSourceID;
  protected
    function GetItem(const AIndex: MVSourceID): IMSource; virtual;
    function FreeItemID: MVSourceID;
    function _RemoveItem(const AID: MVSourceID): IMSource;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure _SetCount(const ACount: MVSourceID);
    procedure _SetItem(const AItem: IMSource; const AIndex: MVSourceID); virtual;
    procedure _MarkDeleted(const AIndex: MVSourceID);
    function IterFirst: TMVSourceIterator;
    function IterNext(var Iterator: TMVSourceIterator): IMSource;
    function AddItem(const AItem: IMSource): MVSourceID; virtual;
    procedure DeleteItem(const AID: MVSourceID);
    function RemoveItem(const AID: MVSourceID): IMSource; virtual;
    procedure Clear;
    procedure SoftClear;
    function Empty: Boolean;
    function FindIndex(const AData): MVSourceID;
    function Find(const AData): IMSource;
    function SetToID(const AID: MVSourceID; AItem: IMSource): IMSource; virtual;
    function ValidID(const AID: MVSourceID): Boolean;
    property Count: MVSourceID read GetCount;
    property IsEqual: TSrcDataCompareFunc read FIsEqual write FIsEqual;
    property Items[const AIndex: MVSourceID]: IMSource read GetItem; default;
  end;

implementation

{TSrcUIDArray}

constructor TSrcUIDArray.Create;
begin
  inherited Create;
  FFreeItems:=TLList.Create(SizeOf(MVSourceID));
  FFreeItems.OnCanBefore:=@ItemCanBefore;
  FIsEqual:=nil;
end;

destructor TSrcUIDArray.Destroy;
begin
  Clear;
  FFreeItems.Destroy;
  inherited Destroy;
end;

procedure TSrcUIDArray._SetCount(const ACount: MVSourceID);
begin
  SetLength(FItems,ACount);
end;

procedure TSrcUIDArray._SetItem(const AItem: IMSource; const AIndex: MVSourceID);
begin
  FItems[AIndex]:=AItem;
end;

procedure TSrcUIDArray._MarkDeleted(const AIndex: MVSourceID);
begin
  FItems[AIndex]:=nil;
  FFreeItems.InsertSorted(AIndex);
end;

function TSrcUIDArray.IterFirst: TMVSourceIterator;
begin
  Result:=-1;
end;

function TSrcUIDArray.IterNext(var Iterator: TMVSourceIterator): IMSource;
var
  L: MVSourceID;
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

function TSrcUIDArray.ItemCanBefore(const ANew,AExisting): Boolean; stdcall;
var
  ANew2     : MVSourceID absolute ANew;
  AExisting2: MVSourceID absolute AExisting;
begin
  Result:=ANew2<AExisting2;
end;

function TSrcUIDArray.GetCount: MVSourceID;
begin
  Result:=Length(FItems);
end;

function TSrcUIDArray.GetItem(const AIndex: MVSourceID): IMSource;
begin
  Result:=FItems[AIndex];
end;

function TSrcUIDArray.FreeItemID: MVSourceID;
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

function TSrcUIDArray.AddItem(const AItem: IMSource): MVSourceID;
begin
  Result:=FreeItemID;
  FItems[Result]:=AItem;
end;

function TSrcUIDArray._RemoveItem(const AID: MVSourceID): IMSource;
var
  AItem           : ^IMSource;
  L,ADelID,ALDelID: MVSourceID;
begin
  AItem:=@FItems[AID];
  Result:=AItem^;
  L:=Length(FItems);
  if AID<L-1 then begin
    FFreeItems.InsertSorted(AID);
    AItem^:=nil;
  end else begin
    AItem^:=nil;
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

procedure TSrcUIDArray.DeleteItem(const AID: MVSourceID);
begin
  _RemoveItem(AID);
end;

function TSrcUIDArray.RemoveItem(const AID: MVSourceID): IMSource;
begin
  Result:=_RemoveItem(AID);
end;

procedure TSrcUIDArray.Clear;
var
  I    : Integer;
  AItem: IMSource;
begin
  for I:=0 to Length(FItems)-1 do begin
    AItem:=FItems[I];
    if AItem<>nil then FItems[I]:=nil;
  end;
  SetLength(FItems,0);
  FFreeItems.Clear;
end;

procedure TSrcUIDArray.SoftClear;
begin
  SetLength(FItems,0);
  FFreeItems.Clear;
end;

function TSrcUIDArray.Empty: Boolean;
begin
  Result:=(Length(FItems)=0);
end;

function TSrcUIDArray.FindIndex(const AData): MVSourceID;
var
  I: Cardinal;
begin
  for I:=0 to Length(FItems)-1 do if FIsEqual(Items[I],AData) then begin
    Result:=I;
    exit;
  end;
  Result:=-1;
end;

function TSrcUIDArray.Find(const AData): IMSource;
var
  I: Cardinal;
begin
  for I:=0 to Length(FItems)-1 do begin
    Result:=Items[I];
    if FIsEqual(Result,AData) then exit;
  end;
  Result:=nil;
end;

function TSrcUIDArray.SetToID(const AID: MVSourceID; AItem: IMSource): IMSource;
begin
  if AID>=Length(FItems) then begin
    SetLength(FItems,AID+1);
    Result:=nil;
  end else Result:=Items[AID];
  _SetItem(AItem,AID);
  //FItems[AID]:=AItem;
end;

function TSrcUIDArray.ValidID(const AID: MVSourceID): Boolean;
begin
  Result:=((AID>=0) and (AID<Length(FItems)));
  if Result then Result:=FItems[AID]<>nil;
end;

end.

