unit LockedObjects;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObjectClasses, ObjectClassBasic;

type
  TLockedObjects = class
  strict private
    FLock          : TMultiReadExclusiveWriteSynchronizer;
    FObjects       : TObjectList;
    FOnDeleteObject: TNotifyEvent;
    procedure BeforeAutomaticDeletion(Sender: TObject);
    procedure AutomaticDeletion(Sender: TObject);
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure IterateObjects(var AIterator: TObjectListItem);
    function AddObject(AObject: TObjectItem): Boolean;
    //Removes Object only, does not destroy it
    function RemoveObject(AObject: TObjectItem): Boolean;
    property Count: Integer read GetCount;
    property OnDeleteObject: TNotifyEvent read FOnDeleteObject write FOnDeleteObject;
  end;

implementation

{%REGION TLockedObjects}

constructor TLockedObjects.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
  FObjects:=TObjectList.Create;
  FObjects.OnBeforeAutomaticDeletion:=@BeforeAutomaticDeletion;
  FObjects.OnAutomaticDeletion:=@AutomaticDeletion;
end;

destructor TLockedObjects.Destroy;
begin
  FObjects.Destroy;
  FLock.Destroy;
  inherited Destroy;
end;

procedure TLockedObjects.BeforeAutomaticDeletion(Sender: TObject);
begin
  FLock.Beginwrite;
end;

procedure TLockedObjects.AutomaticDeletion(Sender: TObject);
begin
  FLock.Endwrite;
  if Assigned(FOnDeleteObject)
    then FOnDeleteObject(Self);
end;

function TLockedObjects.GetCount: Integer;
begin
  FLock.Beginread;
  Result:=FObjects.Count;
  FLock.Endread;
end;

procedure TLockedObjects.IterateObjects(var AIterator: TObjectListItem);
begin
  if AIterator = nil then begin
    FLock.Beginread;
    AIterator:=FObjects.First;
  end else AIterator:=AIterator.Next;
  if AIterator = nil
    then FLock.Endread;
end;

function TLockedObjects.AddObject(AObject: TObjectItem): Boolean;
var
  AItem: TObjectListItem;
begin
  FLock.Beginwrite;

  //check if already there
  AItem:=FObjects.First;
  while AItem<>nil do begin
    if AItem.Content = AObject then begin
      FLock.EndWrite;
      Result:=false;
      exit;
    end;
    AItem:=AItem.Next;
  end;
  //insert
  FObjects.Add(AObject);

  FLock.EndWrite;
  Result:=true;
end;

function TLockedObjects.RemoveObject(AObject: TObjectItem): Boolean;
var
  AItem: TObjectListItem;
begin
  FLock.Beginwrite;

  AItem:=FObjects.First;
  while AItem<>nil do begin
    if AItem.Content = AObject then begin
      AItem.Destroy;
      AutomaticDeletion(nil);
      Result:=true;
      exit;
    end;
    AItem:=AItem.Next;
  end;

  AutomaticDeletion(nil);
  Result:=false;
end;

{%ENDREGION}

end.

