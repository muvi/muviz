unit OrderedObjectList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClassBasic, ObjectClasses;

type
  TOrderedObjectList = class (TObjectList)
  protected
    function Before(a, b: TObjectItem): Boolean; virtual;
  public
    procedure Add(AObject: TObjectItem = nil); override;
  end;

implementation

{%REGION TOrderedObjectList}

procedure TOrderedObjectList.Add(AObject: TObjectItem = nil);
var
  AListItem: TObjectListItem;
begin
  AListItem:=First;
  while (AListItem <> nil) do begin
    if Before(AObject, AListItem.Content) then begin
      AListItem.AddPrev(AObject);
      exit;
    end;
    AListItem:=AListItem.Next;
  end;
  inherited Add(AObject);
end;

function TOrderedObjectList.Before(a, b: TObjectItem): Boolean;
begin
  Result:=PtrInt(a) < PtrInt(b);
end;

{%ENDREGION}

end.

