unit SimpleNotificationList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TSimpleNotification     = procedure of object;

  TSimpleNotificationList = class
  private
    //unsorted array of notifications.
    //sorting will have no benefit because of reordering after removal
    FNotifications: array of TSimpleNotification;
    FLock         : TMultiReadExclusiveWriteSynchronizer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(ANotification: TSimpleNotification);
    function Remove(ANotification: TSimpleNotification): Boolean;
    procedure Notify;
  end;

implementation

{%REGION TSimpleNotificationList}

constructor TSimpleNotificationList.Create;
begin
  inherited Create;
  FLock:=TMultiReadExclusiveWriteSynchronizer.Create;
end;

destructor TSimpleNotificationList.Destroy;
begin
  SetLength(FNotifications, 0);
  FLock.Destroy;
  inherited Destroy;
end;

procedure TSimpleNotificationList.Add(ANotification: TSimpleNotification);
var
  L: Integer;
begin
  FLock.Beginwrite;
  L:=Length(FNotifications);
  SetLength(FNotifications, L+1);
  FNotifications[L]:=ANotification;
  FLock.Endwrite;
end;

function TSimpleNotificationList.Remove(ANotification: TSimpleNotification): Boolean;
var
  I,L        : Integer;
  ATmp, ATmp2: TSimpleNotification;
begin
  FLock.Beginwrite;
  L:=Length(FNotifications)-1;
  I:=L;
  while I>=0 do begin
    //swap with last
    ATmp:=FNotifications[I];
    FNotifications[I]:=ATmp2;
    ATmp2:=ATmp;
    //check if ready
    if ATmp=ANotification then begin
      SetLength(FNotifications, L);
      FLock.Endwrite;
      Result:=true;
      exit;
    end;
    Dec(I);
  end;
  //if element not found:
  //reinsert first element at last position
  FNotifications[L]:=ATmp2;
  FLock.Endwrite;
  Result:=false;
end;

procedure TSimpleNotificationList.Notify;
var
  I            : Integer;
  ANotification: TSimpleNotification;
begin
  FLock.Beginread;
  I:=0;
  //do not save Length in L, it may change
  while I < Length(FNotifications) do begin
    ANotification:=FNotifications[I];

    FLock.Endread;
    ANotification();
    Inc(I);
    FLock.Beginread;
  end;
  FLock.Endread;
end;

{%ENDREGION}

end.

