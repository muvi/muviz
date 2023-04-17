unit ObjectArray;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClassBasic, ObjectClasses;

type
  TObjectArray     = class (TBasicObjectArray)
  private
    FItems      : array of TObjectArrayItem;
  strict protected
    function GetItem(Index: Cardinal): TObjectItem; override;
    procedure SetItem(Index: Cardinal; Value: TObjectItem); override;
    function GetCount: Cardinal; override;
    //dot nor implement: procedure SetCount(Value: Cardinal);
  protected
    procedure ItemDeleted(AIndex: Cardinal); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure Add(Value: TObjectItem);
    procedure Insert(AIndex: Cardinal; Value: TObjectItem);
  end;

implementation

{%REGION TObjectArray}

destructor TObjectArray.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TObjectArray.GetItem(Index: Cardinal): TObjectItem;
begin
  Assert(Index < Length(FItems));
  Result:=FItems[Index].Content;
end;

procedure TObjectArray.SetItem(Index: Cardinal; Value: TObjectItem);
begin
  Assert(Index < Length(FItems));
  Assert(Value <> nil);
  if FItems[Index]<>nil
    then FItems[Index].Destroy;
  FItems[Index]:=TObjectArrayItem.Create(Self, Index, Value, OwnsObjects);
end;

function TObjectArray.GetCount: Cardinal;
begin
  Result:=Length(FItems);
end;

procedure TObjectArray.ItemDeleted(AIndex: Cardinal);
var
  I, L: Integer;
begin
  L:=Length(FItems);
  Assert(AIndex < L);
  FItems[AIndex].Destroy;
  for I:=AIndex to L-2 do begin
    FItems[I]:=FItems[I+1];
    //reset index
    FItems[I].Index:=I;
  end;
  SetLength(FItems, L-1);
end;

procedure TObjectArray.Clear;
var
  I: Integer;
begin
  for I:=0 to Length(FItems)-1
    do FItems[I].Destroy;
  SetLength(FItems, 0);
end;

procedure TObjectArray.Add(Value: TObjectItem);
var
  L: Integer;
begin
  L:=Length(FItems);
  SetLength(FItems, L+1);
  FItems[L]:=TObjectArrayItem.Create(Self, L, Value, OwnsObjects);
end;

procedure TObjectArray.Insert(AIndex: Cardinal; Value: TObjectItem);
var
  I, L: Integer;
begin
  L:=Length(FItems);
  Assert(AIndex <= L);
  SetLength(FItems, L+1);
  for I:=L-1 downto AIndex+1 do begin
    FItems[I]:=FItems[I-1];
    //reset index
    FItems[I].Index:=I;
  end;
  FItems[AIndex]:=TObjectArrayItem.Create(Self, AIndex, Value, OwnsObjects);
end;

{%ENDREGION}

end.

